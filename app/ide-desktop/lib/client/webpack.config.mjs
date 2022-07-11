import path, { dirname } from 'node:path'
import { fileURLToPath } from 'node:url'
import { createRequire } from 'node:module'

import webpack from 'webpack'
import CopyPlugin from 'copy-webpack-plugin'
import { require_env } from '../../utils.mjs'

const thisPath = path.resolve(dirname(fileURLToPath(import.meta.url)))
const dist = path.resolve(require_env('ENSO_BUILD_IDE'), 'client')
const project_manager_in_bundle_path = require_env('ENSO_BUILD_PROJECT_MANAGER_IN_BUNDLE_PATH')
const bundled_engine_version = process.env['ENSO_BUILD_IDE_BUNDLED_ENGINE_VERSION']

export default {
    entry: {
        index: path.resolve(thisPath, 'src', 'index.js'),
        preload: path.resolve(thisPath, 'src', 'preload.cjs'),
    },
    mode: 'production',
    target: 'electron-main',
    output: {
        path: dist,
        filename: '[name].cjs',
    },
    plugins: [
        new CopyPlugin({
            patterns: [
                {
                    from: path.resolve(thisPath, 'package.json'),
                    to: path.join(dist, 'package.json'),
                },
            ],
            options: {},
        }),
        new webpack.DefinePlugin({
            BUNDLED_ENGINE_VERSION: JSON.stringify(bundled_engine_version),
            PROJECT_MANAGER_IN_BUNDLE_PATH: JSON.stringify(project_manager_in_bundle_path),
        }),
    ],
    performance: {
        hints: false,
    },
    stats: 'minimal',
}
