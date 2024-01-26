import init, {
  is_ident_or_operator,
  parse_doc_to_json,
  parse as parse_tree,
} from '../../rust-ffi/pkg/rust_ffi'
import { isNode } from '../util/detect'

if (isNode) {
  const fs = await import('node:fs/promises')
  const buffer = fs.readFile('./rust-ffi/pkg/rust_ffi_bg.wasm')
  await init(buffer)
} else {
  await init()
}

// eslint-disable-next-line camelcase
export { is_ident_or_operator, parse_doc_to_json, parse_tree }
