/**
 * @file Provides the Rust ffi interface. The interface should be kept in sync with polyglot ffi inteface {@link module:ffiPolyglot}.
 *
 * @module ffi
 */

import { createXXHash128 } from 'hash-wasm'
import type { IDataType } from 'hash-wasm/dist/lib/util'
import init, { is_ident_or_operator, parse, parse_doc_to_json } from '../../rust-ffi/pkg/rust_ffi'
import { assertDefined } from '../util/assert'
import { isNode } from '../util/detect'

let xxHasher128: Awaited<ReturnType<typeof createXXHash128>> | undefined
export function xxHash128(input: IDataType) {
  assertDefined(xxHasher128, 'Module should have been loaded with `initializeFFI`.')
  xxHasher128.init()
  xxHasher128.update(input)
  return xxHasher128.digest()
}

export async function initializeFFI(path?: string | undefined) {
  if (isNode) {
    const fs = await import('node:fs/promises')
    const { fileURLToPath, URL: nodeURL } = await import('node:url')
    const buffer = fs.readFile(
      path ?? fileURLToPath(new nodeURL('../../rust-ffi/pkg/rust_ffi_bg.wasm', import.meta.url)),
    )
    await init(buffer)
  } else {
    await init()
  }
  xxHasher128 = await createXXHash128()
}

// TODO[ao]: We cannot to that, because the ffi is used by cjs modules.
// await initializeFFI()

/* eslint-disable-next-line camelcase */
export { is_ident_or_operator, parse_doc_to_json, parse as parse_tree }
