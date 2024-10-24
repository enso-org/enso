/**
 * @file This module defines a TS script that is responsible for invoking the Electron Builder
 * process to bundle the entire IDE distribution.
 *
 * There are two areas to this:
 * - Parsing CLI options as per our needs.
 * - The default configuration of the build process.
 */
/** @module */

import * as childProcess from 'node:child_process'
import * as fs from 'node:fs/promises'

import * as electronNotarize from '@electron/notarize'
import * as electronBuilder from 'electron-builder'
import yargs from 'yargs'

import * as common from 'enso-common'

import * as fileAssociations from './fileAssociations'
import * as paths from './paths'
import computeHashes from './tasks/computeHashes.mjs'
import signArchivesMacOs from './tasks/signArchivesMacOs'

import BUILD_INFO from './buildInfo'

// =============
// === Types ===
// =============

/**
 * The parts of the electron-builder configuration that we want to keep configurable.
 * @see `args` definition below for fields description.
 */
export interface Arguments {
  // The types come from a third-party API and cannot be changed.
  readonly target?: string | undefined
  readonly iconsDist: string
  readonly guiDist: string
  readonly ideDist: string
  readonly projectManagerDist: string
  readonly platform: electronBuilder.Platform
  readonly sign: boolean
}

/** File association configuration, extended with information needed by the `enso-installer`. */
interface ExtendedFileAssociation extends electronBuilder.FileAssociation {
  /**
   * The Windows registry key under which the file association is registered.
   *
   * Should follow the pattern `Enso.CamelCaseName`.
   */
  readonly progId: string
}

/** Additional configuration for the installer. */
interface InstallerAdditionalConfig {
  /** The company name to be used in the installer. */
  readonly publisher: string

  /** File association configuration. */
  readonly fileAssociations: ExtendedFileAssociation[]
}

//======================================
// === Argument parser configuration ===
//======================================

/**
 * CLI argument parser (with support for environment variables) that provides
 * the necessary options.
 */
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
    sign: {
      type: 'boolean',
      description: 'Should signing/notarization be performed (defaults to true)',
      default: true,
    },
  }).argv

// ======================================
// === Electron builder configuration ===
// ======================================

/** File associations for the IDE. */
export const EXTENDED_FILE_ASSOCIATIONS = [
  {
    ext: `.${fileAssociations.SOURCE_FILE_EXTENSION}`,
    name: `${common.PRODUCT_NAME} Source File`,
    role: 'Editor',
    // Note that MIME type is used on Windows by the enso-installer to register the file association.
    // This behavior is unlike what electron-builder does.
    mimeType: 'text/plain',
    progId: 'Enso.Source',
  },
  {
    ext: `.${fileAssociations.BUNDLED_PROJECT_EXTENSION}`,
    name: `${common.PRODUCT_NAME} Project Bundle`,
    role: 'Editor',
    mimeType: 'application/gzip',
    progId: 'Enso.ProjectBundle',
  },
]

/**
 * Returns non-extended file associations, as required by the `electron-builder`.
 *
 * Note that we need to actually remove any additional fields that we added to the file associations,
 * as the `electron-builder` will error out if it encounters unknown fields.
 */
function getFileAssociations(): electronBuilder.FileAssociation[] {
  return EXTENDED_FILE_ASSOCIATIONS.map(assoc => {
    const { ext, name, role, mimeType } = assoc
    return { ext, name, role, mimeType }
  })
}

/** Returns additional configuration for the `enso-installer`. */
function getInstallerAdditionalConfig(): InstallerAdditionalConfig {
  return {
    publisher: common.COMPANY_NAME,
    fileAssociations: EXTENDED_FILE_ASSOCIATIONS,
  }
}

/** Based on the given arguments, creates a configuration for the Electron Builder. */
export function createElectronBuilderConfig(passedArgs: Arguments): electronBuilder.Configuration {
  let version = BUILD_INFO.version
  if (passedArgs.target === 'msi') {
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
      // This provides extra data for the installer.
      installer: getInstallerAdditionalConfig(),
    },
    copyright: `Copyright © ${new Date().getFullYear()} ${common.COMPANY_NAME}`,

    // Note that the `artifactName` uses the "canonical" version of the product, not one that might have been
    // simplified for the MSI installer to cope.
    artifactName: 'enso-${os}-${arch}-' + BUILD_INFO.version + '.${ext}',

    /**
     * Definitions of URL {@link electronBuilder.Protocol} schemes used by the IDE.
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
     * https://www.electronjs.org/docs/latest/tutorial/launch-app-from-url-in-another-app.
     */
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
      target: (passedArgs.target as any) ?? 'dmg',
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
      target: passedArgs.target ?? 'dir',
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
    fileAssociations: getFileAssociations(),
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
    afterPack: (context: electronBuilder.AfterPackContext) => {
      if (passedArgs.platform === electronBuilder.Platform.MAC) {
        // Make the subtree writable, so we can sign the binaries.
        // This is needed because GraalVM distribution comes with read-only binaries.
        childProcess.execFileSync('chmod', ['-R', 'u+w', context.appOutDir])
      }
    },

    afterSign: async (context: electronBuilder.AfterPackContext) => {
      // Notarization for macOS.
      if (
        passedArgs.platform === electronBuilder.Platform.MAC &&
        process.env.CSC_LINK != null &&
        passedArgs.sign
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
          entitlements: macConfig!.entitlements!,
          identity: 'Developer ID Application: New Byte Order Sp. z o. o. (NM77WTZJFQ)',
        })

        console.log('  • Notarizing.')

        await electronNotarize.notarize({
          tool: 'notarytool',
          appPath: `${appOutDir}/${appName}.app`,
          // It is a mistake for either of these to be undefined.
          appleId: process.env.APPLEID!,
          appleIdPassword: process.env.APPLEIDPASS!,
          teamId: process.env.APPLETEAMID!,
        })
      }
    },

    publish: null,
  }
}

/**
 * Write the configuration to a JSON file.
 *
 * On Windows it is necessary to provide configuration to our installer. On other platforms, this may be useful for debugging.
 *
 * The configuration will be extended with additional information needed by the `enso-installer`.
 */
async function dumpConfiguration(configPath: string, config: electronBuilder.Configuration) {
  const jsonConfig = JSON.stringify(config)
  await fs.writeFile(configPath, jsonConfig)
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

  const config = createElectronBuilderConfig(passedArgs)
  const cliOpts: electronBuilder.CliOptions = {
    config,
    targets: passedArgs.platform.createTarget(),
  }

  // If `ENSO_BUILD_ELECTRON_BUILDER_CONFIG` is set, we will write the configuration to the
  // specified path. Otherwise, we will write it to the default path.
  // This is used on Windows to provide the configuration to the installer build. On other
  // platforms, this may be useful for debugging.
  const configPath =
    process.env['ENSO_BUILD_ELECTRON_BUILDER_CONFIG'] ??
    `${passedArgs.ideDist}/electron-builder-config.yaml`
  console.log(`Writing configuration to ${configPath}`)
  await dumpConfiguration(configPath, config)

  console.log('Building with configuration:', cliOpts)
  const result = await electronBuilder.build(cliOpts)
  console.log('Electron Builder is done. Result:', result)
  // FIXME: https://github.com/enso-org/enso/issues/6082
  // This is a workaround which fixes esbuild hanging after successfully finishing
  // `electronBuilder.build`. It is safe to `exit(0)` since all processes are finished.
  process.exit(0)
}
