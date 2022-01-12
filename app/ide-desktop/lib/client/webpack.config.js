const Copy = require('copy-webpack-plugin')
const path = require('path')
const paths = require('../../../../build/paths')

const thisPath = path.resolve(__dirname)

module.exports = {
    entry: {
        index: path.resolve(thisPath, 'src', 'index.js'),
    },
    mode: 'production',
    target: 'electron-main',
    output: {
        path: paths.dist.content,
        filename: '[name].js',
    },
    plugins: [
        new Copy([
            {
                from: path.resolve(thisPath, 'package.json'),
                to: paths.dist.packageJson,
            },
            {
                from: path.resolve(thisPath, 'src', 'preload.js'),
                to: paths.dist.preload,
            },
        ]),
    ],
    performance: {
        hints: false,
    },
    stats: 'minimal',
}
