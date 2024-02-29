/***
 * String interpolation handling. Code in this module must be kept aligned with lexer's
 * understanding  of string escapes. The relevant lexer code can be found in
 * `lib/rust/parser/src/lexer.rs`, search for `fn text_escape`.
 */

import { assertUnreachable } from 'shared/util/assert'

const interpolationMap = [
  ['0', '\0'],
  ['a', '\x07'],
  ['b', '\x08'],
  ['f', '\x0C'],
  ['n', '\x0A'],
  ['r', '\x0D'],
  ['t', '\x09'],
  ['v', '\x0B'],
  ['e', '\x1B'],
  ['\\', '\\'],
  ['"', '"'],
  ["'", "'"],
  ['`', '`'],
] as const

function escapeAsCharCodes(str: string): string {
  let out = ''
  for (let i = 0; i < str.length; i += 1) out += `\\u{${str?.charCodeAt(i).toString(16)}}`
  return out
}

const escapeRegex = new RegExp(
  `${interpolationMap.map(([_, raw]) => escapeAsCharCodes(raw)).join('|')}`,
  'gu',
)

const applyRegex = new RegExp(
  '\\\\(?:' +
    `${interpolationMap.map(([escape]) => escapeAsCharCodes(escape)).join('|')}` +
    '|x[0-9a-fA-F]{0,2}' +
    '|u\\{[0-9a-fA-F]{0,4}\\}?' + // lexer allows the trailing } to be missing
    '|u[0-9a-fA-F]{0,4}' +
    '|U[0-9a-fA-F]{0,8}' +
    ')',
  'gu',
)

const escapeMapping = Object.fromEntries(
  interpolationMap.map(([escape, raw]) => [raw, `\\${escape}`]),
)
const applyMapping = Object.fromEntries(
  interpolationMap.map(([escape, raw]) => [`\\${escape}`, raw]),
)

/** Escape a string so it can be safely spliced into an interpolated (`''`) Enso string.
 * NOT USABLE to insert into raw strings. */
export function escapeInterpolation(rawString: string) {
  return rawString.replace(escapeRegex, (match) => escapeMapping[match] ?? assertUnreachable())
}

/** Interpret all escaped characters from an interpolated (`''`) Enso string. */
export function applyInterpolation(escapedString: string) {
  return escapedString.replace(applyRegex, (match) => {
    let cut = 2
    switch (match[1]) {
      case 'u':
        if (match[2] === '{') cut = 3 // fallthrough
      case 'U':
      case 'x':
        return String.fromCharCode(parseInt(match.substring(cut), 16))
      default:
        return applyMapping[match] ?? assertUnreachable()
    }
  })
}
