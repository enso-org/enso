import * as terms from './generated/parser.terms'

const idents: Record<string, number> = {
  and: terms.AND,
  between: terms.BETWEEN,
  else: terms.ELSE,
  empty: terms.EMPTY,
  end: terms.END,
  false: terms.FALSE,
  if: terms.IF,
  in: terms.IN,
  is: terms.IS,
  like: terms.LIKE,
  not: terms.NOT,
  nothing: terms.NOTHING,
  null: terms.NULL,
  or: terms.OR,
  then: terms.THEN,
  true: terms.TRUE,
}

// noinspection JSUnusedGlobalSymbols
/** Case-insensitive token specializer recognizing keywords. */
export function specializeIdent(value: string, _stack: unknown): number {
  return idents[value.toLowerCase()] ?? -1
}
