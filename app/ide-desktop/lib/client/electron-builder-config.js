// This file cannot be made ES6 module due to: https://github.com/develar/read-config-file/issues/10

const dist_var_name = "ENSO_IDE_DIST"

const dist = process.env[dist_var_name] ?? (()=>{throw Error(`Missing ${dist_var_name} environment variable.`)})()

const build = require('../../build.json')

const config = {
    appId: 'org.enso',
    productName: 'Enso',
    extraMetadata: {
        version: build.version
    },
    copyright: 'Copyright © 2021 ${author}.',
    artifactName: 'enso-${os}-${version}.${ext}',
    mac: {
        // We do not use compression as the build time is huge and file size saving is almost zero.
        target: ['dmg'],
        icon: `${dist}/icons/icon.icns`,
        category: 'public.app-category.developer-tools',
        darkModeSupport: true,
        type: 'distribution',
        // The following settings are required for macOS signing and notarisation.
        // The hardened runtime is required to be able to notarise the application.
        hardenedRuntime: true,
        // This is a custom check that is not working correctly, so we disable it. See for more
        // details https://kilianvalkhof.com/2019/electron/notarizing-your-electron-application/
        gatekeeperAssess: false,
        // Location of the entitlements files with the entitlements we need to run our application
        // in the hardened runtime.
        entitlements: './entitlements.mac.plist',
        entitlementsInherit: './entitlements.mac.plist',
    },
    win: {
        // We do not use compression as the build time is huge and file size saving is almost zero.
        target: ['nsis'],
        icon: `${dist}/icons/icon.ico`,
    },
    linux: {
        // We do not use compression as the build time is huge and file size saving is almost zero.
        target: ['AppImage'],
        icon: `${dist}/icons/png`,
        category: 'Development',
    },
    files: [{ from: `${dist}/content/`, to: '.' }],
    extraResources: [{ from: `${dist}/project-manager/`, to: '.', filter: ['!**.tar.gz', '!**.zip'] }],
    fileAssociations: [
        {
            ext: 'enso',
            name: 'Enso Source File',
            role: 'Editor',
        },
    ],
    directories: {
        output: `${dist}/client`,
    },
    nsis: {
        differentialPackage: false,
    },
    dmg: {
        writeUpdateInfo: false,
        sign: false,
    },
}

module.exports = config
