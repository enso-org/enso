/***
 * String escaping and interpolation handling. Code in this module must be kept aligned with lexer's
 * understanding  of string literals. The relevant lexer code can be found in
 * `lib/rust/parser/src/lexer.rs`, search for `fn text_escape`.
 */

import { assertUnreachable } from '../util/assert'

const escapeSequences = [
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
  `${escapeSequences.map(([_, raw]) => escapeAsCharCodes(raw)).join('|')}`,
  'gu',
)

const unescapeRegex = new RegExp(
  '\\\\(?:' +
    `${escapeSequences.map(([escape]) => escapeAsCharCodes(escape)).join('|')}` +
    '|x[0-9a-fA-F]{0,2}' +
    '|u\\{[0-9a-fA-F]{0,4}\\}?' + // Lexer allows trailing } to be missing.
    '|u[0-9a-fA-F]{0,4}' +
    '|U[0-9a-fA-F]{0,8}' +
    ')',
  'gu',
)

const escapeMapping = Object.fromEntries(
  escapeSequences.map(([escape, raw]) => [raw, `\\${escape}`]),
)
const unescapeMapping = Object.fromEntries(
  escapeSequences.map(([escape, raw]) => [`\\${escape}`, raw]),
)

/**
 * Escape a string so it can be safely spliced into an interpolated (`''`) Enso string.
 * Note: Escape sequences are NOT interpreted in raw (`""`) string literals.
 * */
export function escapeTextLiteral(rawString: string) {
  return rawString.replace(escapeRegex, (match) => escapeMapping[match] ?? assertUnreachable())
}

/**
 * Interpret all escaped characters from an interpolated (`''`) Enso string.
 * Note: Escape sequences are NOT interpreted in raw (`""`) string literals.
 */
export function unescapeTextLiteral(escapedString: string) {
  return escapedString.replace(unescapeRegex, (match) => {
    let cut = 2
    switch (match[1]) {
      case 'u':
        if (match[2] === '{') cut = 3 // fallthrough
      case 'U':
      case 'x':
        return String.fromCharCode(parseInt(match.substring(cut), 16))
      default:
        return unescapeMapping[match] ?? assertUnreachable()
    }
  })
}
