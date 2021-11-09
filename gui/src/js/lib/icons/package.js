let config = {
    name: 'enso-studio-icons',
    version: '1.0.0',
    scripts: {
        build: 'node src/index.js',
    },
    devDependencies: {
        sharp: '^0.26.2',
        'to-ico': '^1.1.5',
    },
}

module.exports = { config }
