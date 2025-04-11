/***
 * String escaping and interpolation handling. Code in this module must be kept aligned with lexer's
 * understanding  of string literals. The relevant lexer code can be found in
 * `lib/rust/parser/src/lexer.rs`, search for `fn text_escape`.
 */

import { assertDefined } from '../util/assert'
import { TextLiteral } from './tree'

const baseEscapes = [
  ['0', '\0'],
  ['a', '\x07'],
  ['b', '\x08'],
  ['f', '\x0C'],
  ['r', '\x0D'],
  ['t', '\x09'],
  ['v', '\x0B'],
  ['e', '\x1B'],
  ['\\', '\\'],
  ['`', '`'],
] as const
const inlineEscapes = [...baseEscapes, ['n', '\x0A'] as const, ["'", "'"] as const]
const blockEscapes = baseEscapes
const allEscapes = [...inlineEscapes, ...blockEscapes]

function escapeAsCharCodes(str: string): string {
  let out = ''
  for (let i = 0; i < str.length; i += 1) out += `\\u{${str?.charCodeAt(i).toString(16)}}`
  return out
}

function escapeRegex(sequences: readonly (readonly [string, string])[]) {
  return new RegExp(
    [
      ...sequences.map(([_, raw]) => escapeAsCharCodes(raw)),
      // Unpaired-surrogate codepoints are not technically valid in Unicode, but they are allowed in Javascript strings.
      // Enso source files must be strictly UTF-8 conformant.
      '\\p{Surrogate}',
    ].join('|'),
    'gu',
  )
}
const inlineEscapeRegex = escapeRegex(inlineEscapes)
const blockEscapeRegex = escapeRegex(blockEscapes)

const escapeMapping = Object.fromEntries(allEscapes.map(([escape, raw]) => [raw, `\\${escape}`]))

function escapeChar(char: string) {
  const fixedEscape = escapeMapping[char]
  if (fixedEscape != null) return fixedEscape
  return escapeAsCharCodes(char)
}

/**
 * Escape a string so it can be safely spliced into an interpolated (`'` or `'''`) Enso string.
 * Note: Escape sequences are NOT interpreted in raw (`"` or `"""`) string literals.
 */
export function escapeTextLiteral(rawString: string, isBlock: boolean = false) {
  return rawString.replace(isBlock ? blockEscapeRegex : inlineEscapeRegex, escapeChar)
}

/**
 * Interpret all escaped characters from an interpolated (`''`) Enso string, provided without open/close delimiters.
 * Note: Escape sequences are NOT interpreted in raw (`""`) string literals.
 */
export function unescapeTextLiteral(escapedString: string) {
  const ast = TextLiteral.tryParse("'" + escapedString + "'")
  assertDefined(ast)
  return ast.rawTextContent
}
