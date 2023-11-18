/** @file This module defines a TS script that is responsible for invoking the Electron Builder
 * process to bundle the entire IDE distribution.
 *
 * There are two areas to this:
 * - Parsing CLI options as per our needs.
 * - The default configuration of the build process. */
/** @module */

import * as childProcess from 'node:child_process'
import * as fs from 'node:fs/promises'

import * as electronBuilder from 'electron-builder'
import * as electronNotarize from '@electron/notarize'
import type * as macOptions from 'app-builder-lib/out/options/macOptions'
import yargs from 'yargs'

import * as common from 'enso-common'

import * as fileAssociations from './file-associations'
import * as paths from './paths'
import computeHashes from './tasks/computeHashes.mjs'
import signArchivesMacOs from './tasks/signArchivesMacOs'

import BUILD_INFO from '../../../../build.json' assert { type: 'json' }

// =============
// === Types ===
// =============

/** The parts of the electron-builder configuration that we want to keep configurable.
 *
 * @see `args` definition below for fields description. */
export interface Arguments {
    // The types come from a third-party API and cannot be changed.
    // eslint-disable-next-line no-restricted-syntax
    target?: string | undefined
    iconsDist: string
    guiDist: string
    ideDist: string
    projectManagerDist: string
    platform: electronBuilder.Platform
}

//======================================
// === Argument parser configuration ===
//======================================

/** CLI argument parser (with support for environment variables) that provides
 * the necessary options. */
export const args: Arguments = await yargs(process.argv.slice(2))
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
            default: electronBuilder.Platform.current().toString(),
            coerce: (p: string) => electronBuilder.Platform.fromString(p),
        },
        target: {
            type: 'string',
            description: 'Overwrite the platform-default target',
        },
    }).argv

// ======================================
// === Electron builder configuration ===
// ======================================

/** Based on the given arguments, creates a configuration for the Electron Builder. */
export function createElectronBuilderConfig(passedArgs: Arguments): electronBuilder.Configuration {
    let version = BUILD_INFO.version
    if (
        passedArgs.target === 'msi' ||
        (passedArgs.target == null && process.platform === 'win32')
    ) {
        // MSI installer imposes some restrictions on the version number. Namely, product version must have a major
        // version less than 256, a minor version less than 256, and a build version less than 65536.
        //
        // As a workaround (we use year, like 2023, as a major version), we drop two leading digits from the major
        // version number.
        version = version.substring(2)
    }

    return {
        appId: 'org.enso',
        productName: common.PRODUCT_NAME,
        extraMetadata: {
            version,
        },
        copyright: `Copyright © ${new Date().getFullYear()} ${common.COMPANY_NAME}`,

        // Note that the `artifactName` uses the "canonical" version of the product, not one that might have been
        // simplified for the MSI installer to cope.
        artifactName: 'enso-${os}-' + BUILD_INFO.version + '.${ext}',

        /** Definitions of URL {@link electronBuilder.Protocol} schemes used by the IDE.
         *
         * Electron will register all URL protocol schemes defined here with the OS.
         * Once a URL protocol scheme is registered with the OS, any links using that scheme
         * will function as "deep links".
         * Deep links are used to redirect the user from external sources (e.g., system web browser,
         * email client) to the IDE.
         *
         * Clicking a deep link will:
         * - open the IDE (if it is not already open),
         * - focus the IDE, and
         * - navigate to the location specified by the URL of the deep link.
         *
         * For details on how this works, see:
         * https://www.electronjs.org/docs/latest/tutorial/launch-app-from-url-in-another-app. */
        protocols: [
            /** Electron URL protocol scheme definition for deep links to authentication pages. */
            {
                name: `${common.PRODUCT_NAME} url`,
                schemes: [common.DEEP_LINK_SCHEME],
                role: 'Editor',
            },
        ],
        mac: {
            // Compression is not used as the build time is huge and file size saving
            // almost zero.
            // This type assertion is UNSAFE, and any users MUST verify that
            // they are passing a valid value to `target`.
            // eslint-disable-next-line no-restricted-syntax
            target: (passedArgs.target ?? 'dmg') as macOptions.MacOsTargetName,
            icon: `${passedArgs.iconsDist}/icon.icns`,
            category: 'public.app-category.developer-tools',
            darkModeSupport: true,
            type: 'distribution',
            // The following settings are required for macOS signing and notarisation.
            // The hardened runtime is required to be able to notarise the application.
            hardenedRuntime: true,
            // This is a custom check that is not working correctly, so we disable it. See for more
            // details https://kilianvalkhof.com/2019/electron/notarizing-your-electron-application/
            gatekeeperAssess: false,
            // Location of the entitlements files with the entitlements we need to run
            // our application in the hardened runtime.
            entitlements: './entitlements.mac.plist',
            entitlementsInherit: './entitlements.mac.plist',
        },
        win: {
            // Compression is not used as the build time is huge and file size saving
            // almost zero.
            target: passedArgs.target ?? 'nsis',
            icon: `${passedArgs.iconsDist}/icon.ico`,
        },
        linux: {
            // Compression is not used as the build time is huge and file size saving
            // is almost zero.
            target: passedArgs.target ?? 'AppImage',
            icon: `${passedArgs.iconsDist}/png`,
            category: 'Development',
        },
        files: [
            '!**/node_modules/**/*',
            { from: `${passedArgs.guiDist}/`, to: '.' },
            { from: `${passedArgs.ideDist}/client`, to: '.' },
        ],
        extraResources: [
            {
                from: `${passedArgs.projectManagerDist}/`,
                to: paths.PROJECT_MANAGER_BUNDLE,
                filter: ['!**.tar.gz', '!**.zip'],
            },
        ],
        fileAssociations: [
            {
                ext: fileAssociations.SOURCE_FILE_EXTENSION,
                name: `${common.PRODUCT_NAME} Source File`,
                role: 'Editor',
            },
            {
                ext: fileAssociations.BUNDLED_PROJECT_EXTENSION,
                name: `${common.PRODUCT_NAME} Project Bundle`,
                role: 'Editor',
            },
        ],
        directories: {
            output: `${passedArgs.ideDist}`,
        },
        msi: {
            runAfterFinish: false,
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
            runAfterFinish: false,
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
        afterAllArtifactBuild: computeHashes,
        afterPack: ctx => {
            if (passedArgs.platform === electronBuilder.Platform.MAC) {
                // Make the subtree writable, so we can sign the binaries.
                // This is needed because GraalVM distribution comes with read-only binaries.
                childProcess.execFileSync('chmod', ['-R', 'u+w', ctx.appOutDir])
            }
        },

        afterSign: async context => {
            // Notarization for macOS.
            if (
                passedArgs.platform === electronBuilder.Platform.MAC &&
                process.env.CSC_LINK != null
            ) {
                const {
                    packager: {
                        appInfo: { productFilename: appName },
                        config: { mac: macConfig },
                    },
                    appOutDir,
                } = context

                // We need to manually re-sign our build artifacts before notarization.
                console.log('  • Performing additional signing of dependencies.')
                await signArchivesMacOs({
                    appOutDir: appOutDir,
                    productFilename: appName,
                    // This will always be defined since we have an `entitlements.mac.plist`.
                    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                    entitlements: macConfig!.entitlements!,
                    identity: 'Developer ID Application: New Byte Order Sp. z o. o. (NM77WTZJFQ)',
                })

                console.log('  • Notarizing.')

                await electronNotarize.notarize({
                    tool: 'notarytool',
                    appPath: `${appOutDir}/${appName}.app`,
                    // It is a mistake for either of these to be undefined.
                    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                    appleId: process.env.APPLEID!,
                    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                    appleIdPassword: process.env.APPLEIDPASS!,
                    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                    teamId: process.env.APPLETEAMID!,
                })
            }
        },

        publish: null,
    }
}

/** Build the IDE package with Electron Builder. */
export async function buildPackage(passedArgs: Arguments) {
    // `electron-builder` checks for presence of `node_modules` directory. If it is not present, it
    // will install dependencies with the`--production` flag(erasing all dev - only dependencies).
    // This does not work sensibly with NPM workspaces. We have our `node_modules` in
    // the root directory, not here.
    //
    // Without this workaround, `electron-builder` will end up erasing its own dependencies and
    // failing because of that.
    await fs.mkdir('node_modules', { recursive: true })

    const cliOpts: electronBuilder.CliOptions = {
        config: createElectronBuilderConfig(passedArgs),
        targets: passedArgs.platform.createTarget(),
    }
    console.log('Building with configuration:', cliOpts)
    const result = await electronBuilder.build(cliOpts)
    console.log('Electron Builder is done. Result:', result)
    // FIXME: https://github.com/enso-org/enso/issues/6082
    // This is a workaround which fixes esbuild hanging after successfully finishing
    // `electronBuilder.build`. It is safe to `exit(0)` since all processes are finished.
    process.exit(0)
}
