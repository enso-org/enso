const Copy = require('copy-webpack-plugin')
const path = require('path')

const thisPath = path.resolve(__dirname)
const root = path.resolve(thisPath, '..', '..', '..', '..')
const distPath = path.resolve(root, 'dist')

module.exports = {
    entry: {
        index: path.resolve(thisPath, 'src', 'index.js'),
    },
    mode: 'production',
    target: 'electron-main',
    output: {
        path: path.resolve(distPath, 'content'),
        filename: '[name].js',
    },
    plugins: [
        new Copy([
            {
                from: path.resolve(thisPath, 'package.json'),
                to: path.resolve(distPath, 'content', 'package.json'),
            },
            {
                from: path.resolve(thisPath, 'src', 'preload.js'),
                to: path.resolve(distPath, 'content', 'preload.js'),
            },
        ]),
    ],
    performance: {
        hints: false,
    },
    stats: 'minimal',
}
