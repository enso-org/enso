import init, { parse, parse_doc_to_json } from 'rust-ffi/pkg/rust_ffi'

if (RUNNING_VITEST) {
  const fs = await import('node:fs/promises')
  const buffer = await fs.readFile('./rust-ffi/pkg/rust_ffi_bg.wasm')
  await init(buffer)
} else {
  await init()
}

// eslint-disable-next-line camelcase
export { parse, parse_doc_to_json }
