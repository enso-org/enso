let config = {
    name: "enso-studio-content",
    scripts: {
        "build": "npx webpack",
        "watch": "npx webpack-dev-server"
    },
    dependencies: {
        "enso-studio-common": "2.0.0-alpha.0",
        "copy-webpack-plugin": "^5.1.1"
    },
    devDependencies: {
        "compression-webpack-plugin": "^3.1.0",
        "copy-webpack-plugin": "^5.1.1"
    }
}

module.exports = {config}
