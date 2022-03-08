const Copy = require('copy-webpack-plugin')
const path = require('path')
const paths = require('../../../../build/paths')

const thisPath = path.resolve(__dirname)

const dist = path.resolve(process.env.ENSO_IDE_DIST ?? '../../../../dist')
const distContent = path.join(dist, 'content')

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
                to: path.join(distContent, 'package.json')
            },
            {
                from: path.resolve(thisPath, 'src', 'preload.js'),
                to: path.join(distContent, 'preload.js'),
            },
        ]),
    ],
    performance: {
        hints: false,
    },
    stats: 'minimal',
}
