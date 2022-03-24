const Copy = require('copy-webpack-plugin')
const path = require('path')
const utils = require('../../utils')

const thisPath = path.resolve(__dirname)
const dist = path.resolve(utils.require_env("ENSO_BUILD_IDE"), "client")

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
        new Copy([
            {
                from: path.resolve(thisPath, 'package.json'),
                to: path.join(dist, 'package.json')
            },
            {
                from: path.resolve(thisPath, 'src', 'preload.js'),
                to: path.join(dist, 'preload.js'),
            },
        ]),
    ],
    performance: {
        hints: false,
    },
    stats: 'minimal',
}
