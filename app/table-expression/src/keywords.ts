import * as terms from './generated/parser.terms'

const keywords = [
  'AND',
  'BETWEEN',
  'ELSE',
  'EMPTY',
  'END',
  'FALSE',
  'IF',
  'IN',
  'IS',
  'LIKE',
  'NOT',
  'NOTHING',
  'NULL',
  'OR',
  'THEN',
  'TRUE',
] as const

const idents = new Map<string, number>(keywords.map((k) => [k, terms[k]]))

// noinspection JSUnusedGlobalSymbols
/** Case-insensitive token specializer recognizing keywords. */
export function specializeIdent(value: string, _stack: unknown): number {
  return idents.get(value.toUpperCase()) ?? -1
}
