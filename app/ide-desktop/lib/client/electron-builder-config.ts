/**
 * This module defines a TS script that is responsible for invoking the Electron Builder process to
 * bundle the entire IDE distribution.
 *
 * There are two areas to this:
 * - Parsing CLI options as per our needs.
 * - The default configuration of the build process.
 *
 * @module
 */

import path from 'node:path'
import fs from 'node:fs/promises'
import { CliOptions, Configuration, LinuxTargetSpecificOptions, Platform } from 'electron-builder'
import builder from 'electron-builder'

import { require_env } from '../../utils.js'
import { project_manager_bundle } from './paths.js'
import build from '../../build.json' assert { type: 'json' }
import yargs from 'yargs'
import { MacOsTargetName } from 'app-builder-lib/out/options/macOptions'

const args = await yargs(process.argv.slice(2))
    .env('ENSO_BUILD')
    .option({
        ideDist: {
            // Alias here (and subsequent occurrences) are for the environment variable name.
            alias: 'ide',
            type: 'string',
            description: 'Output directory for IDE',
            demandOption: true,
        },
        guiDist: {
            alias: 'gui',
            type: 'string',
            description: 'Output directory with GUI',
            demandOption: true,
        },
        iconsDist: {
            alias: 'icons',
            type: 'string',
            description: 'Output directory with icons',
            demandOption: true,
        },
        projectManagerDist: {
            alias: 'project-manager',
            type: 'string',
            description: 'Output directory with project manager',
            demandOption: true,
        },
        platform: {
            type: 'string',
            description: 'Platform that Electron Builder should target',
            default: Platform.current().toString(),
            coerce: (p: string) => Platform.fromString(p),
        },
        targetOverride: {
            type: 'string',
            description: 'Overwrite the platform-default target',
        },
    }).argv

const config: Configuration = {
    appId: 'org.enso',
    productName: 'Enso',
    extraMetadata: {
        version: build.version,
    },
    copyright: 'Copyright © 2022 ${author}.',
    artifactName: 'enso-${os}-${version}.${ext}',
    mac: {
        // We do not use compression as the build time is huge and file size saving is almost zero.
        target: (args.targetOverride as MacOsTargetName) ?? 'dmg',
        icon: `${args.iconsDist}/icon.icns`,
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
        target: args.targetOverride ?? 'nsis',
        icon: `${args.iconsDist}/icon.ico`,
    },
    linux: {
        // We do not use compression as the build time is huge and file size saving is almost zero.
        target: args.targetOverride ?? 'AppImage',
        icon: `${args.iconsDist}/png`,
        category: 'Development',
    },
    files: [
        '!**/node_modules/**/*',
        { from: `${args.guiDist}/`, to: '.' },
        { from: `${args.ideDist}/client`, to: '.' },
    ],
    extraResources: [
        {
            from: `${args.projectManagerDist}/`,
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
        output: `${args.ideDist}`,
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
    afterAllArtifactBuild: path.join('tasks', 'computeHashes.cjs'),

    // TODO [mwu]: Temporarily disabled, signing should be revised.
    //             In particular, engine should handle signing of its artifacts.
    // afterPack: 'tasks/prepareToSign.js',

    publish: null,
}

// `electron-builder` checks for presence of `node_modules` directory. If it is not present, it will
// install dependencies with `--production` flag (erasing all dev-only dependencies). This does not
// work sensibly with NPM workspaces. We have our `node_modules` in the root directory, not here.
//
// Without this workaround, `electron-builder` will end up erasing its own dependencies and failing
// because of that.
await fs.mkdir('node_modules', { recursive: true })

const cli_opts: CliOptions = {
    config: config,
    targets: args.platform.createTarget(),
}

console.log('Building with configuration:', cli_opts)

const result = await builder.build(cli_opts)
console.log('Electron Builder is done. Result:', result)
