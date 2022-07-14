import path, { dirname } from 'node:path'
import { fileURLToPath } from 'node:url'

import esbuild from 'esbuild'
import NodeModulesPolyfillPlugin from '@esbuild-plugins/node-modules-polyfill'

import { require_env } from '../../utils.mjs'
import copy_plugin from 'enso-copy-plugin'

const thisPath = path.resolve(dirname(fileURLToPath(import.meta.url)))
const dist = path.resolve(require_env('ENSO_BUILD_IDE'), 'client')
const project_manager_in_bundle_path = require_env('ENSO_BUILD_PROJECT_MANAGER_IN_BUNDLE_PATH')
const bundled_engine_version = process.env['ENSO_BUILD_IDE_BUNDLED_ENGINE_VERSION']

const entryPoints = ['src/index.js', 'src/preload.cjs']
const define = {
    BUNDLED_ENGINE_VERSION: JSON.stringify(bundled_engine_version),
    PROJECT_MANAGER_IN_BUNDLE_PATH: JSON.stringify(project_manager_in_bundle_path),
}

function filesToCopy() {
    return [path.resolve(thisPath, 'package.json')]
}

const options = {
    bundle: true,
    outdir: dist,
    outbase: 'src',
    platform: 'node',
    plugins: [
        NodeModulesPolyfillPlugin.NodeModulesPolyfillPlugin(),
        // copy_plugin.create(files_to_copy_provider)
    ],
    entryPoints,
    define,
    external: ['emitter'], // Conditionally required dependency only for non-node environment by `batch` package.
}

await esbuild.build(options)
