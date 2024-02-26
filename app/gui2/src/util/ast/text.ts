import { swapKeysAndValues } from '@/util/record'

const mapping: Record<string, string> = {
  '\b': '\\b',
  '\f': '\\f',
  '\n': '\\n',
  '\r': '\\r',
  '\t': '\\t',
  '\v': '\\v',
  '\\': '\\\\',
  '"': '\\"',
  "'": "\\'",
  '`': '``',
}

const reverseMapping = swapKeysAndValues(mapping)

/** Escape a string so it can be safely spliced into an interpolated (`''`) Enso string.
 * NOT USABLE to insert into raw strings. Does not include quotes. */
export function escape(string: string) {
  return string.replace(/[\0\b\f\n\r\t\v\\"'`]/g, (match) => mapping[match]!)
}

/** The reverse of `escape`: transform the string into human-readable form, not suitable for interpolation. */
export function unescape(string: string) {
  return string.replace(/\\[0bfnrtv\\"']|``/g, (match) => reverseMapping[match]!)
}
