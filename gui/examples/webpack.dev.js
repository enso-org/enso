const merge  = require('webpack-merge');
const common = require('./webpack.common.js');

module.exports = merge(common, {
    mode: 'development',
    devtool: '',
    devServer: {
        contentBase: './dist',
    },
});
