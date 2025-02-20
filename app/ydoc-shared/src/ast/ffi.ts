/**
 * @file Provides the Rust ffi interface. The interface should be kept in sync with polyglot ffi inteface {@link module:ffiPolyglot}.
 * @module ffi
 */

import { createXXHash128 } from 'hash-wasm'
import type { IDataType } from 'hash-wasm/dist/lib/util'
import {
  is_ident_or_operator,
  is_numeric_literal,
  parse_block,
  parse_doc_to_json,
  parse_module,
} from 'rust-ffi'

const xxHasher128 = await createXXHash128()
/** Return the xxhash hash for the given buffer. */
export function xxHash128(input: IDataType) {
  xxHasher128.init()
  xxHasher128.update(input)
  return xxHasher128.digest()
}

/* eslint-disable-next-line camelcase */
export { is_ident_or_operator, is_numeric_literal, parse_block, parse_doc_to_json, parse_module }
