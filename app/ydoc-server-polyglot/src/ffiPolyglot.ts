/**
 * @file This file is used as rust ffi interface for building the polyglot ydoc server.
 * All the exported methods are provided by the ydoc server implementation.
 * The interface should be kept in sync with Rust ffi interface {@link ydoc-shared/src/ast/ffi}.
 * @module ffiPolyglot
 */

export const {
  is_ident_or_operator,
  is_numeric_literal,
  parse_doc_to_json,
  parse_block,
  parse_module,
  xxHash128,
} = globalThis
