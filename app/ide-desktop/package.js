let config = {
    name: 'root',
    version: '1.0.0',
    private: true,
    devDependencies: {
        lerna: '^3.20.2',
        webpack: '^4.42.1',
        'webpack-cli': '^3.3.11',
        'webpack-dev-server': '^3.1.5',
    },
    scripts: {
        // IMPORTANT! Please remember to run "bootstrap" before "build" or "watch" to install
        // or update the required dependencies.
        bootstrap: 'npx lerna bootstrap',
        build: 'npx lerna run build --stream',
        clean: 'npx lerna clean -y && rm -Rf node_modules',
        dist: 'npx lerna run build --stream && npx lerna run dist --stream',
        install: 'npm install lerna && npx lerna bootstrap',
        start: 'npm run build && npx lerna run start --stream -- -- ',
        watch: 'npx lerna run watch --stream',
    },
}

module.exports = { config }
