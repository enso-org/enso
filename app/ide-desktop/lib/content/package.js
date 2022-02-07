let config = {
    name: 'enso-studio-content',
    version: '1.0.0',
    scripts: {
        build: 'npx webpack',
        watch: 'npx webpack-dev-server',
    },
    dependencies: {
        'enso-studio-common': '1.0.0',
        firebase: '^8.6.8',
        firebaseui: '^4.8.0',
        'copy-webpack-plugin': '^5.1.1',
        'html-loader': '^1.3.2',
        'mixpanel-browser': '2.40.1',
    },
    devDependencies: {
        'compression-webpack-plugin': '^3.1.0',
        'copy-webpack-plugin': '^5.1.1',
        'yaml-loader': '^0.6.0',
        'source-map-loader': '^1.0.0',
        'ts-loader': '^8.0.3',
        typescript: '^4.0.2',
        webpack: '^4.44.1',
        'webpack-cli': '^3.3.12',
    },
}

module.exports = { config }
