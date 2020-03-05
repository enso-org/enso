const Copy = require('copy-webpack-plugin')

const path = require('path')
const root = path.resolve(__dirname)

module.exports = {
    entry: {
        index: path.resolve(root,'src','index.js'),
    },
    mode: 'production',
    target: "electron-main",
    output: {
        path: path.resolve(root,'dist','unpacked'),
        filename: '[name].js',
    },
    plugins: [
        new Copy([
            {
                from : path.resolve(root,'..','content','dist','assets'),
                to   : path.resolve(root,'dist','unpacked','assets')
            },
            {
                from : path.resolve(root,'..','icons','dist'),
                to   : path.resolve(root,'dist','icons')
            },
            {
                from : path.resolve('package.json'),
                to   : path.resolve(root,'dist','unpacked','package.json')
            },
            {
                from : path.resolve('src/preload.js'),
                to   : path.resolve(root,'dist','unpacked','preload.js')
            }
        ]),
    ],
    performance: {
        hints: false,
    },
    stats: 'minimal',
}
