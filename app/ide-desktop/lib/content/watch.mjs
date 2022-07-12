import * as bundle from './bundle2.mjs'

import * as server from 'enso-gui-server'

const bundle_promise = bundle.bundle()

const bundle_result = await bundle_promise
console.log("First bundle ready, will spawn server.", bundle_result)
const root = bundle.output_path
const assets = root;
await server.start(root, assets)
