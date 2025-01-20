export const getCellValueType = (item: string) => {
  switch (true) {
    case isInteger(item):
      return 'Integer'
    case isDate(item):
      return 'Date'
    case isTime(item):
      return 'Time'
    case isDateTime(item):
      return 'Date_Time'
    default:
      return 'Char'
  }
}

export const isInteger = (item: string) => {
  return !isNaN(Number(item))
}

export const isDate = (item: string) => {
  return /^\d{4}-\d{2}-\d{2}$/.test(item)
}

export const isTime = (item: string) => {
  return /^([01]\d|2[0-3]):([0-5]\d):([0-5]\d)(\.\d{1,6})?$/.test(item)
}

export const isDateTime = (item: string) => {
  return /^\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12]\d|3[01]) ([01]\d|2[0-3]):([0-5]\d):([0-5]\d)(\.\d{1,6})?(\[[+-]\d{1,3}(:[0-5]\d)?\])?$/.test(
    item,
  )
}

export const isNumericType = (valueType: string) => {
  const isNumber = ['Integer', 'Float', 'Decimal', 'Byte']
  return isNumber.indexOf(valueType) != -1
}
