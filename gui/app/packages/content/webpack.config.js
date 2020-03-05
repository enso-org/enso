const CopyWebpackPlugin = require('copy-webpack-plugin')
const CompressionPlugin = require('compression-webpack-plugin')

const path = require('path')
const root = path.resolve(__dirname)

module.exports = {
    entry: {
        index: path.resolve(root,'src','index.js'),
        wasm_imports: './src/wasm_imports.js',
    },
    output: {
        path: path.resolve(root,'dist','assets'),
        filename: '[name].js',
        libraryTarget: 'umd',
    },
    node: {
        fs: 'empty'
    },
    plugins: [
        new CompressionPlugin(),
        new CopyWebpackPlugin([
            path.resolve(root,'src','index.html'),
            path.resolve(root,'..','..','generated','wasm','gui.wasm'),
        ]),
    ],
    devServer: {
        publicPath: '/assets/',
        historyApiFallback: {
            index: '/assets/'
        }
    },
    resolve: {
        alias: {
            wasm_rust_glue$: path.resolve(root,'..','..','generated','wasm','gui.js')
        }
    },
    performance: {
        hints: false,
    },
    mode: 'development',
    stats: 'minimal'
}
