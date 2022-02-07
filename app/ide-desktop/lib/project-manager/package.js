let config = {
    name: 'enso-studio-project-manager',
    version: '1.0.0',
    scripts: {
        build: 'npx ts-node src/build.ts',
    },

    devDependencies: {
        unzipper: '^0.10',
        tar: '^6.0.5',
        'follow-redirects': '^1.13',
        typescript: `^3.4`,
    },
}

module.exports = { config }
