import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { IServerSideGetRowsRequest } from 'ag-grid-community'
import {
  actionMap,
  FilterAction,
  getFilterValue,
  GridFilterModel,
  makeFilterModelList,
} from './tableVizFilterUtils'
import { getCellValueType } from './tableVizUtils'

type ValueTypes = 'Date' | 'Time' | 'Date_Time' | 'Integer' | 'Char'
type ValueTypeArgumentChild = { valueType: ValueTypes; value: string }
type ValueTypeArgumentParent =
  | { valueType: ValueTypes; value: string }
  | { valueType: 'Mixed'; value: ValueTypeArgumentChild[] }
type PossibleArguments = string | ValueTypeArgumentParent
export type Argument = string | Array<PossibleArguments>
type SortDirection = 'asc' | 'desc'
const sortDirectionMap = {
  asc: '1',
  desc: '-1',
}

const parseSingleArgument = (
  value: ValueTypeArgumentParent,
  tempModule: Ast.MutableModule,
): Ast.Owned<Ast.MutableExpression> => {
  switch (value.valueType) {
    case 'Date': {
      const datePattern = Pattern.parseExpression('(Date.new __ __ __)')
      const dateParts = value.value
        .match(/\d+/g)!
        .slice(0, 3)
        .map((part: string) => Ast.tryNumberToEnso(Number(part), tempModule)!)
      return datePattern.instantiateCopied(dateParts)
    }
    case 'Time': {
      const pattern = Pattern.parseExpression('Time_Of_Day.parse (__)')!
      return pattern.instantiateCopied([Ast.TextLiteral.new(value.value, tempModule)])
    }
    case 'Date_Time': {
      const pattern = Pattern.parseExpression('Date_Time.parse (__)')!
      return pattern.instantiateCopied([Ast.TextLiteral.new(value.value, tempModule)])
    }
    case 'Integer':
      return Ast.parseExpression(value.value, tempModule)!
    case 'Char':
      return Ast.TextLiteral.new(value.value)
    case 'Mixed': {
      const items = value.value.map((val: { valueType: ValueTypes; value: string }) =>
        parseSingleArgument(val, tempModule),
      )
      return Ast.Vector.new(tempModule, items)
    }
    default:
      return Ast.parseExpression(value, tempModule)!
  }
}

export const parseArgument = (arg: Argument, tempModule: Ast.MutableModule) => {
  if (Array.isArray(arg)) {
    const itemList = arg.map((i) => {
      return typeof i === 'string' ?
          Ast.parseExpression(i, tempModule)!
        : parseSingleArgument(i, tempModule)
    })
    return Ast.Vector.new(tempModule, itemList!)
  }
  return Ast.parseExpression(arg, tempModule)!
}

export const convertSortModel = (request: IServerSideGetRowsRequest, columnHeaders: string[]) => {
  const sortColIndexesMap = request.sortModel.map((sortCol) => {
    return `${columnHeaders.findIndex((h: string) => sortCol.colId === h)}`
  })
  const sortColIndexes = sortColIndexesMap.length ? sortColIndexesMap : 'Nothing'
  const sortDirections =
    sortColIndexesMap.length ?
      request.sortModel.map((sortCol) => {
        return sortDirectionMap[sortCol.sort as SortDirection]
      })
    : 'Nothing'
  return { sortColIndexes, sortDirections }
}

export const convertFilterModel = (
  request: IServerSideGetRowsRequest,
  columnHeaders: string[],
  colTypeMap: Map<string, string>,
) => {
  const gridFilterModelList: Array<GridFilterModel> =
    request.filterModel ? makeFilterModelList(request.filterModel) : []

  const filterColumnNames = gridFilterModelList.map((filter) => filter.columnName)

  const filterColumnIndexList =
    filterColumnNames.length ?
      filterColumnNames.map((colName) => `${columnHeaders.findIndex((h: string) => colName === h)}`)
    : 'Nothing'

  const filterActions =
    filterColumnNames.length ?
      gridFilterModelList.map((filter) => {
        return filter.filterType === 'set' ?
            '..Is_In'
          : actionMap[filter.filterAction as FilterAction]
      })
    : 'Nothing'

  const valueMap = gridFilterModelList.map((filter) => {
    return {
      valType: colTypeMap.get(filter.columnName),
      action: actionMap[filter.filterAction as FilterAction],
      value: getFilterValue(filter),
    }
  })

  const valueList: Argument =
    valueMap.length ?
      valueMap.map((value) => {
        if (value.valType === 'Mixed' && Array.isArray(value.value)) {
          const parseValues = value.value.map((val) => {
            return { valueType: getCellValueType(val), value: val }
          })
          return { valueType: 'Mixed', value: parseValues } as ValueTypeArgumentParent
        }

        if (
          value.action === '..Between' &&
          typeof value.value === 'object' &&
          'fromValue' in value.value
        ) {
          return { valueType: value.valType as ValueTypes, value: `${value.value.fromValue}` }
        }

        return { valueType: value.valType as ValueTypes, value: `${value.value}` }
      })
    : 'Nothing'

  return { filterColumnIndexList, filterActions, valueList }
}

export const createExpressionTemplate = (
  visulizationModule: string,
  expressionString: string,
  ...positionalArgumentsExpressions: Argument[]
) => {
  const tempModule = Ast.MutableModule.Transient()
  const preprocessorModule = Ast.parseExpression(visulizationModule, tempModule)!
  const preprocessorQn = Ast.PropertyAccess.new(
    tempModule,
    preprocessorModule,
    Ast.identifier(expressionString)!,
  )

  const preprocessorInvocation = Ast.App.PositionalSequence(preprocessorQn, [
    Ast.Wildcard.new(tempModule),
    ...positionalArgumentsExpressions.map((arg) => {
      const parsedArg = parseArgument(arg, tempModule)
      return Ast.Group.new(tempModule, parsedArg)
    }),
  ])
  return (nodeId: string) => {
    const rhs = Ast.parseExpression(nodeId, tempModule)!
    return Ast.OprApp.new(tempModule, preprocessorInvocation, '<|', rhs)
  }
}
