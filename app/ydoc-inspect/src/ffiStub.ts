/**
 * @file Stub for the Rust FFI interface. ydoc-inspect only reads AST from
 * synced Y.Docs and never parses code, so the parser functions are not needed.
 * FIXME: https://github.com/enso-org/enso/issues/14988
 */

/* eslint-disable camelcase */

function notAvailable(): never {
  throw new Error('Parser FFI is not available in ydoc-inspect')
}

export const is_ident_or_operator = notAvailable as never
export const self_arg_separator = notAvailable as never
export const is_numeric_literal = notAvailable as never
export const parse_block = notAvailable as never
export const parse_module = notAvailable as never
/** Parser FFI stub. Hashing is not used by ydoc-inspect. */
export function xxHash128(): never {
  notAvailable()
}
