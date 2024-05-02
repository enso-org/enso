/**
 * @file This file is used as ffi {@link module:ffi} interface for building the polyglot ydoc server.
 * All the exported methods are provided by the ydoc server implementation.
 * The interface should be kept in sync with Rust ffi inteface {@link module:ffi}.
 *
 * @module ffiPolyglot
 */

import type { IDataType } from 'hash-wasm/dist/lib/util'

declare global {
  function parse_tree(code: string): Uint8Array
  function parse_doc_to_json(docs: string): string
  function is_ident_or_operator(code: string): number
  function xxHash128(input: IDataType): string
}

export async function initializeFFI(_path?: string | undefined) {}

/* eslint-disable-next-line camelcase */
export const { is_ident_or_operator, parse_doc_to_json, parse_tree, xxHash128 } = globalThis
