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

interface FilterOption {
  filterType?: string
  type?: string
  filter?: string
  filterTo?: string
  dateFrom?: string
  dateTo?: string
  values?: string
}

const createFilterModel = (key: string, filter: FilterOption) => ({
  columnName: key,
  filterType: filter.filterType,
  filterAction: filter.type,
  filter: filter.filter,
  filterTo: filter.filterTo,
  dateFrom: filter.dateFrom,
  dateTo: filter.dateTo,
  values: filter.values,
})

export const makeFilterModelList = (gridFilterModel: Record<string, any>) =>
  Object.entries(gridFilterModel).flatMap(([key, value]) => {
    if (value.filterType === 'multi' && Array.isArray(value.filterModels)) {
      return value.filterModels
        .filter((filter: FilterOption) => filter != null)
        .map((filter: FilterOption) => createFilterModel(key, filter))
    }
    return [createFilterModel(key, value)]
  })
