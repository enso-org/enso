import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import type { IServerSideGetRowsRequest } from 'ag-grid-enterprise'
import {
  actionMap,
  type FilterAction,
  getFilterValue,
  type GridFilterModel,
} from './tableVizFilterUtils'
import { getCellValueType } from './tableVizUtils'

export type ValueTypes =
  | 'Date'
  | 'Time'
  | 'Date_Time'
  | 'Integer'
  | 'Char'
  | 'Boolean'
  | 'Float'
  | 'Decimal'
  | 'Byte'
export type ValueTypeArgumentChild = { valueType: ValueTypes; value: string }
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

const parseFilterValues = (
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
    case 'Float':
    case 'Decimal':
    case 'Byte':
      return Ast.parseExpression(value.value, tempModule)!
    case 'Char':
      return Ast.TextLiteral.new(value.value)
    case 'Mixed': {
      const items = value.value.map((val: { valueType: ValueTypes; value: string }) =>
        parseFilterValues(val, tempModule),
      )
      return Ast.Vector.new(tempModule, items)
    }
    case 'Boolean':
      return value.value === 'false' ?
          Ast.Ident.new(tempModule, Ast.identifier('False')!)
        : Ast.Ident.new(tempModule, Ast.identifier('True')!)
    default:
      return Ast.parseExpression(value, tempModule)!
  }
}

export const parseArgument = (arg: string[] | 'Nothing', tempModule: Ast.MutableModule) => {
  if (Array.isArray(arg)) {
    const itemList = arg.map((i) => Ast.parseExpression(i, tempModule)!)
    return Ast.Vector.new(tempModule, itemList!)
  }
  return Ast.parseExpression(arg, tempModule)!
}

const parseFilterCondition = (
  filterAction: string,
  filterValue: any,
  tempModule: Ast.MutableModule,
) => {
  if (filterAction === '..Is_Nothing') {
    return Ast.parseExpression('..Is_Nothing', tempModule)!
  }
  if (filterAction === '..Not_Nothing') {
    return Ast.parseExpression('..Not_Nothing', tempModule)!
  }
  if (filterAction === '..Between') {
    const filterCondition = Pattern.parseExpression('(..Between __ __)')
    const fromValue = parseFilterValues(filterValue[0], tempModule)
    const toValue = parseFilterValues(filterValue[1], tempModule)
    return filterCondition.instantiateCopied([fromValue, toValue])
  }
  const filterCondition = Pattern.parseExpression('(__ __)')
  const action = Ast.parseExpression(filterAction, tempModule)!
  const filterVal = parseFilterValues(filterValue, tempModule)
  return filterCondition.instantiateCopied([action, filterVal])
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
  gridFilterModelList: Array<GridFilterModel>,
  columnHeaders: string[],
  colTypeMap: Map<string, string>,
) => {
  const filterColumnNames = gridFilterModelList.map((filter) => filter.columnName)

  const filterColumnIndexList =
    filterColumnNames.length ?
      filterColumnNames.map((colName) => `${columnHeaders.findIndex((h: string) => colName === h)}`)
    : 'Nothing'

  const getSetFilterAction = (filter: GridFilterModel) =>
    colTypeMap.get(filter.columnName) === 'Boolean' ? '..Equal' : '..Is_In'

  const filterActions =
    filterColumnNames.length ?
      gridFilterModelList.map((filter) => {
        return filter.filterType === 'set' ?
            getSetFilterAction(filter)
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

  const valueList =
    valueMap.length ?
      valueMap.map((value) => {
        if (value.action === undefined && Array.isArray(value.value)) {
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
          return [
            { valueType: value.valType as ValueTypes, value: `${value.value.fromValue}` },
            { valueType: value.valType as ValueTypes, value: `${value.value.toValue}` },
          ]
        }

        return { valueType: value.valType as ValueTypes, value: `${value.value}` }
      })
    : 'Nothing'

  return { filterColumnIndexList, filterActions, valueList }
}

export const createDistinctExpressionTemplate = (
  visulizationModule: string,
  expressionString: string,
  columnIndex: string,
  filterColumnIndexList: string[] | 'Nothing',
  filterActions: string[] | 'Nothing',
  valueList: string[] | 'Nothing',
) => {
  const tempModule = Ast.MutableModule.Transient()
  const preprocessorModule = Ast.parseExpression(visulizationModule, tempModule)!
  const preprocessorQn = Ast.PropertyAccess.new(
    tempModule,
    preprocessorModule,
    Ast.identifier(expressionString)!,
  )

  const parseFilterArgs = (actions: string[] | 'Nothing') => {
    if (actions === 'Nothing') {
      return parseArgument('Nothing', tempModule)
    }
    const filters = actions.map((action: string, index: number) => {
      const value = valueList[index]
      return parseFilterCondition(action, value, tempModule)
    })
    return Ast.Vector.new(tempModule, filters)
  }

  const positionalArgumentsExpressions = [
    Ast.parseExpression(columnIndex, tempModule)!,
    parseArgument(filterColumnIndexList, tempModule),
    parseFilterArgs(filterActions),
  ]

  const preprocessorInvocation = Ast.App.PositionalSequence(preprocessorQn, [
    Ast.Wildcard.new(tempModule),
    ...positionalArgumentsExpressions.map((arg) => {
      return Ast.Group.new(tempModule, arg)
    }),
  ])
  return (nodeId: string) => {
    const rhs = Ast.parseExpression(nodeId, tempModule)!
    return Ast.OprApp.new(tempModule, preprocessorInvocation, '<|', rhs)
  }
}

export const createExpressionRowTemplate = (
  visulizationModule: string,
  expressionString: string,
  startRow: string,
  sortColIndexes: string[] | 'Nothing',
  sortDirections: string[] | 'Nothing',
  filterColumnIndexList: string[] | 'Nothing',
  filterActions: string[] | 'Nothing',
  valueList: string[] | 'Nothing',
) => {
  const tempModule = Ast.MutableModule.Transient()
  const preprocessorModule = Ast.parseExpression(visulizationModule, tempModule)!
  const preprocessorQn = Ast.PropertyAccess.new(
    tempModule,
    preprocessorModule,
    Ast.identifier(expressionString)!,
  )

  const parseFilterArgs = (actions: string[] | 'Nothing') => {
    if (actions === 'Nothing') {
      return parseArgument('Nothing', tempModule)
    }
    const filters = actions.map((action: string, index: number) => {
      const value = valueList[index]
      return parseFilterCondition(action, value, tempModule)
    })
    return Ast.Vector.new(tempModule, filters)
  }

  const parsedfilterConditions = parseFilterArgs(filterActions)

  const positionalArgumentsExpressions = [
    Ast.parseExpression(startRow, tempModule)!,
    parseArgument(sortColIndexes, tempModule),
    parseArgument(sortDirections, tempModule),
    parseArgument(filterColumnIndexList, tempModule),
    parsedfilterConditions,
  ]

  const preprocessorInvocation = Ast.App.PositionalSequence(preprocessorQn, [
    Ast.Wildcard.new(tempModule),
    ...positionalArgumentsExpressions.map((arg) => {
      return Ast.Group.new(tempModule, arg)
    }),
  ])
  return (nodeId: string) => {
    const rhs = Ast.parseExpression(nodeId, tempModule)!
    return Ast.OprApp.new(tempModule, preprocessorInvocation, '<|', rhs)
  }
}
