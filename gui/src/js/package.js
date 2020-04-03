let config = {
    name: "root",
    version: "2.0.0-alpha.0",
    private: true,
    devDependencies: {
        "lerna": "^3.20.2",
        "webpack": "^4.42.1",
        "webpack-cli": "^3.3.11",
        "webpack-dev-server": "^3.1.5",
    },
    scripts: {
        "build": "lerna run build --stream",
        "clean": "rm -Rf node_modules && lerna clean -y",
        "dist": "lerna run build --stream && lerna run dist --stream",
        "install": "npm install lerna && lerna bootstrap",
        "start": "npm run build && lerna run start --stream -- -- ",
        "watch": "lerna run watch --stream"
    }
}

module.exports = {config}
