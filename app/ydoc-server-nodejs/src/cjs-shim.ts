/**
 * This file allows `require` to be used to resolve native node modules
 * while executing in esm context. This is required, because some dependencies
 * are still in commonjs format and use it. We need to run in esm context
 * because wasm loader depends on dynamic import and top-level await.
 */

import { createRequire } from 'node:module'
globalThis.require = createRequire(import.meta.url)
