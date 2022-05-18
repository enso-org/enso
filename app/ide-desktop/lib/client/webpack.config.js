const Copy = require('copy-webpack-plugin')
const path = require('path')
const utils = require('../../utils')
const webpack = require('webpack')
const BUILD_INFO = require("../../build.json");

const thisPath = path.resolve(__dirname)

const dist = path.resolve(utils.require_env('ENSO_BUILD_IDE'), 'client')
const bundled_engine_version = process.env['ENSO_BUILD_IDE_BUNDLED_ENGINE_VERSION']

module.exports = {
    entry: {
        index: path.resolve(thisPath, 'src', 'index.js'),
    },
    mode: 'production',
    target: 'electron-main',
    output: {
        path: dist,
        filename: '[name].js',
    },
    plugins: [
        new Copy({
            patterns: [
                {
                    from: path.resolve(thisPath, 'package.json'),
                    to: path.join(dist, 'package.json'),
                },
                {
                    from: path.resolve(thisPath, 'src', 'preload.js'),
                    to: path.join(dist, 'preload.js'),
                },
            ],
            options: {},
        }),
        new webpack.DefinePlugin({
            BUNDLED_ENGINE_VERSION: JSON.stringify(bundled_engine_version),
        }),
    ],
    performance: {
        hints: false,
    },
    stats: 'minimal',
}
