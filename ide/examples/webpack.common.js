const CopyWebpackPlugin = require("copy-webpack-plugin")
const path = require('path');

const mb = 1024 * 1024;

module.exports = {
    entry: "./bootstrap.js",
    output: {
        path: path.resolve(__dirname, "dist"),
        filename: "bootstrap.js",
    },
    node: {
        fs: 'empty'
    },
    plugins: [
        new CopyWebpackPlugin(['index.html']),
    ],
    devServer: {
        historyApiFallback: {
            index: 'index.html'
        }
    },
    resolve: {
        modules: [path.resolve(__dirname, "node_modules")]
    },
    performance: {
        hints: 'error',
        maxAssetSize: 4.2 * mb,
    },
};
