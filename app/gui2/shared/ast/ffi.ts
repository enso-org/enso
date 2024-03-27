import { isJvm } from '../util/detect'

async function resolveImports() {
  if (isJvm) {
    const javaImpl = await import('./ffiJvm')
    return javaImpl
  } else {
    const wasmImpl = await import('./ffiWasm')
    return wasmImpl
  }
}

const { is_ident_or_operator, parse_doc_to_json, parse_tree, initializeFFI, xxHash128 } =
  await resolveImports()

// TODO[ao]: We cannot to that, because the ffi is used by cjs modules.
// await initializeFFI()

/* eslint-disable-next-line camelcase */
export { initializeFFI, is_ident_or_operator, parse_doc_to_json, parse_tree, xxHash128 }
