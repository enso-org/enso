import type { SetFilterValuesFuncParams } from 'ag-grid-community'
import { isNumericType, type ValueType } from './tableVizUtils'

export const getFilterParams = (
  isSSRM: boolean,
  valueType: ValueType | null | undefined,
  filterType: string | null,
  getFilterValues: (params: SetFilterValuesFuncParams<any, string>) => Promise<void>,
) => {
  const filterOptions = valueType ? getFilterOptions(valueType.constructor) : null
  const filterButtons = valueType ? getFilterButtons(valueType.constructor) : null

  const defaultFilter = {
    maxNumConditions: 1,
    values: isSSRM ? getFilterValues : null,
    filterOptions: filterOptions,
    buttons: filterButtons,
    refreshValuesOnOpen: true,
  }

  return filterType != 'agMultiColumnFilter' ? defaultFilter : (
      {
        filters: [
          {
            filter: 'agTextColumnFilter',
            filterParams: {
              ...defaultFilter,
            },
          },
          {
            filter: 'agSetColumnFilter',
            filterParams: {
              ...defaultFilter,
            },
          },
        ],
      }
    )
}

export const getFilterType = (valueType: string, usingMultiFilter: boolean) => {
  if (valueType === 'Date') {
    return 'agDateColumnFilter'
  } else if (isNumericType(valueType)) {
    return 'agNumberColumnFilter'
  } else if (valueType === 'Char') {
    return usingMultiFilter ? 'agMultiColumnFilter' : 'agTextColumnFilter'
  } else if (valueType === 'Date_Time' || valueType === 'Time_Of_Day' || valueType === 'Mixed') {
    return null
  } else {
    return 'agSetColumnFilter'
  }
}

export const getCellDataType = (valueType: string) => {
  if (valueType === 'Date') {
    return 'date'
  } else if (isNumericType(valueType)) {
    return 'number'
  } else if (valueType === 'Char') {
    return 'text'
  } else if (valueType === 'Boolean') {
    return 'boolean'
  } else {
    return false
  }
}

function getFilterOptions(valueType: string) {
  if (valueType === 'Date') {
    return ['equals', 'notEqual', 'greaterThan', 'lessThan', 'inRange', 'blank', 'notBlank']
  } else if (isNumericType(valueType)) {
    return [
      'equals',
      'notEqual',
      'greaterThan',
      'greaterThanOrEqual',
      'lessThan',
      'lessThanOrEqual',
      'inRange',
      'blank',
      'notBlank',
    ]
  } else if (valueType === 'Char') {
    return ['equals', 'notEqual', 'contains', 'startsWith', 'endsWith', 'blank', 'notBlank']
  } else {
    return null
  }
}
function getFilterButtons(valueType: string) {
  if (valueType === 'Date') {
    return ['apply', 'clear']
  } else {
    return ['clear']
  }
}
