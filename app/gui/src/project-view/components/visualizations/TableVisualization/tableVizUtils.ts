import { LINKABLE_URL_REGEX } from '@/util/link'
import type { TextFormatOptions } from '../TableVisualization.vue'

export interface ValueType {
  constructor: string
  display_text: string
}

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

const replaceLinksWithTag = (str: string) => {
  return str.replace(
    LINKABLE_URL_REGEX,
    (url: string) => `<a href="${url}" target="_blank" class="link">${url}</a>`,
  )
}

export const formatText = (input: string, textFormatterSelected: TextFormatOptions) => {
  const htmlEscaped = input.replaceAll('<', '&lt;').replaceAll('>', '&gt;')

  if (textFormatterSelected === 'off') {
    const replaceLinks = replaceLinksWithTag(htmlEscaped)
    return replaceLinks.replace(/^\s+|\s+$/g, '&nbsp;')
  }

  const partialMappings = {
    '\r': '<span style="color: #df8800">␍</span> <br>',
    '\n': '<span style="color: #df8800;">␊</span> <br>',
    '\t': '<span style="color: #df8800; white-space: break-spaces;">&#8594;  |</span>',
  }
  const fullMappings = {
    '\r': '<span style="color: #df8800">␍</span> <br>',
    '\n': '<span style="color: #df8800">␊</span> <br>',
    '\t': '<span style="color: #df8800; white-space: break-spaces;">&#8594;  |</span>',
  }

  const replaceSpaces =
    textFormatterSelected === 'full' ?
      htmlEscaped.replaceAll(' ', '<span style="color: #df8800">&#183;</span>')
    : htmlEscaped.replace(/ \s+|^ +| +$/g, function (match: string) {
        return `<span style="color: #df8800">${match.replaceAll(' ', '&#183;')}</span>`
      })

  const replaceLinks = replaceLinksWithTag(replaceSpaces)

  const replaceReturns = replaceLinks.replace(
    /\r\n/g,
    '<span style="color: #df8800">␍␊</span> <br>',
  )

  const renderOtherWhitespace = (match: string) => {
    return textFormatterSelected === 'full' && match != ' ' ?
        '<span style="color: #df8800">&#9744;</span>'
      : match
  }
  const newString = replaceReturns.replace(/[\s]/g, function (match: string) {
    const mapping = textFormatterSelected === 'full' ? fullMappings : partialMappings
    return mapping[match as keyof typeof mapping] || renderOtherWhitespace(match)
  })
  return `<span > ${newString} <span>`
}
