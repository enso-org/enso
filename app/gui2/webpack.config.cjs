const path = require('path')

module.exports = {
    mode: 'none',
    entry: './ydoc-server/server.ts',
    output: {
        filename: 'ydoc-server-bundle.js',
        path: path.resolve(__dirname, '../../target')
    },
    resolve: {
        extensions: [".ts", ".js"]
    },
    module: {
        rules: [
            {
                test: /\.ts$/,
                loader: "ts-loader"
            }
        ]
    }
}
