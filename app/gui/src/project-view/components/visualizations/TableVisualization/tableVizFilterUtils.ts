import { FilterModel } from 'ag-grid-community'

export type FilterType = 'number' | 'date' | 'set' | 'text'

/**
 * Represents the value used for filtering.
 *
 * - For comparisons such as 'equals' or 'greater than,' the filter value is a single value (string).
 * - For 'is in' filtering, the filter value is a list of strings.
 * - For range filtering, the filter value consists of two values that define the range.
 */
export type FilterValue = string | string[] | FilterValueRange

export const actionMap = {
  equals: '..Equal',
  notEqual: '..Not_Equal',
  greaterThan: '..Greater',
  greaterThanOrEqual: '..Equal_Or_Greater',
  lessThan: '..Less',
  lessThanOrEqual: '..Equal_Or_Less',
  inRange: '..Between',
  blank: '..Is_Nothing',
  notBlank: '..Not_Nothing',
  contains: '..Contains',
  startsWith: '..Starts_With',
  endsWith: '..Ends_With',
}
export type FilterAction = keyof typeof actionMap
export type GridFilterModel = {
  columnName: string
  filterType: FilterType
  filter?: string
  filterTo?: string
  dateFrom?: string
  dateTo?: string
  values?: string[]
  filterAction?: FilterAction
}
export type FilterValueRange = {
  toValue: string
  fromValue: string
}

export const getFilterValue = (filterModel: GridFilterModel) => {
  const filterType = filterModel.filterType
  const filterAction = filterModel.filterAction
  let value: FilterValue
  switch (filterType) {
    case 'number':
      value =
        filterAction === 'inRange' ?
          { toValue: filterModel.filterTo!, fromValue: filterModel.filter! }
        : (filterModel.filter as FilterValue)
      break
    case 'date':
      value =
        filterAction === 'inRange' ?
          { toValue: filterModel.dateTo!, fromValue: filterModel.dateFrom! }
        : (filterModel.dateFrom as FilterValue)
      break
    case 'text':
      value = filterModel.filter as FilterValue
      break
    default:
      value = filterModel.values as FilterValue
  }
  return value
}

export const makeFilterModelList = (gridFilterModel: FilterModel) =>
  Object.entries(gridFilterModel).map(([key, value]) => {
    return {
      columnName: key,
      filterType: value.filterType,
      filterAction: value.type,
      filter: value.filter,
      filterTo: value.filterTo,
      dateFrom: value.dateFrom,
      dateTo: value.dateTo,
      values: value.values,
    }
  })
