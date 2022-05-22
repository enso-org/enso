// This file cannot be made ES6 module due to: https://github.com/develar/read-config-file/issues/10

import path from 'node:path'
import fs from 'node:fs'

import { require_env } from '../../utils.mjs'
import { project_manager_bundle } from './paths.mjs'
import build from '../../build.json' assert { type: 'json' }

const dist = require_env('ENSO_BUILD_IDE')
const gui = require_env('ENSO_BUILD_GUI')
const icons = require_env('ENSO_BUILD_ICONS')
const project_manager = require_env('ENSO_BUILD_PROJECT_MANAGER')

const config = {
    appId: 'org.enso',
    productName: 'Enso',
    extraMetadata: {
        version: build.version,
    },
    copyright: 'Copyright © 2022 ${author}.',
    artifactName: 'enso-${os}-${version}.${ext}',
    mac: {
        // We do not use compression as the build time is huge and file size saving is almost zero.
        target: ['dmg'],
        icon: `${icons}/icon.icns`,
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
        icon: `${icons}/icon.ico`,
    },
    linux: {
        // We do not use compression as the build time is huge and file size saving is almost zero.
        target: ['AppImage'],
        icon: `${icons}/png`,
        category: 'Development',
    },
    files: [
        '!**/node_modules/**/*',
        { from: `${gui}/`, to: '.' },
        { from: `${dist}/client`, to: '.' },
    ],
    extraResources: [
        {
            from: `${project_manager}/`,
            to: project_manager_bundle,
            filter: ['!**.tar.gz', '!**.zip'],
        },
    ],
    fileAssociations: [
        {
            ext: 'enso',
            name: 'Enso Source File',
            role: 'Editor',
        },
    ],
    directories: {
        output: `${dist}`,
    },
    nsis: {
        // Disables "block map" generation during electron building. Block maps
        // can be used for incremental package update on client-side. However,
        // their generation can take long time (even 30 mins), so we removed it
        // for now. Moreover, we may probably never need them, as our updates
        // are handled by us. More info:
        // https://github.com/electron-userland/electron-builder/issues/2851
        // https://github.com/electron-userland/electron-builder/issues/2900
        differentialPackage: false,
    },
    dmg: {
        // Disables "block map" generation during electron building. Block maps
        // can be used for incremental package update on client-side. However,
        // their generation can take long time (even 30 mins), so we removed it
        // for now. Moreover, we may probably never need them, as our updates
        // are handled by us. More info:
        // https://github.com/electron-userland/electron-builder/issues/2851
        // https://github.com/electron-userland/electron-builder/issues/2900
        writeUpdateInfo: false,
        // Disable code signing of the final dmg as this triggers an issue
        // with Apple’s Gatekeeper. Since the DMG contains a signed and
        // notarised application it will still be detected as trusted.
        // For more details see step (4) at
        // https://kilianvalkhof.com/2019/electron/notarizing-your-electron-application/
        sign: false,
    },
    afterAllArtifactBuild: path.join('tasks', 'computeHashes.js'),

    // TODO [mwu]: Temporarily disabled, signing should be revised.
    //             In particular, engine should handle signing of its artifacts.
    // afterPack: 'tasks/prepareToSign.js',
}

fs.writeFileSync('electron-builder-config.json', JSON.stringify(config, null, 2))
