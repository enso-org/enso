/**
 * Exports the electron-builder configuration as JSON for use by the Rust installer build.
 * Output is written to stdout and captured by Bazel's js_run_binary stdout parameter.
 *
 * NOTE: This is likely a temporary solution while we have a Rust-based installer that expects
 * certain format of the electron-builder config.
 *
 * Usage: node export-config.mjs <path-to-config.cjs>
 */

import { createRequire } from 'node:module'
import path from 'node:path'
import process from 'node:process'

const configPath = process.argv[2]
if (!configPath) {
  console.error('Usage: node export-config.mjs <path-to-config.cjs>')
  process.exit(1)
}

const require = createRequire(import.meta.url)

const config = require(path.resolve(configPath))

// Map file associations to the extended format required by the Rust installer.
// The Rust code expects: { progId, mimeType, ext, name }.
function toExtendedFileAssociation(fa) {
  // Generate progId from extension (e.g., ".enso" -> "Enso.Source").
  const extName = fa.ext.replace(/^\./, '') // Remove leading dot.
  const progId = `Enso.${extName.charAt(0).toUpperCase() + extName.slice(1)}`
  return {
    progId,
    mimeType: fa.mimeType || 'application/octet-stream',
    ext: fa.ext,
    name: fa.name,
  }
}

// Build the installer-specific config that the Rust code expects
const installerSpecificConfig = {
  publisher: 'Enso International, Inc.',
  fileAssociations: (config.fileAssociations || []).map(toExtendedFileAssociation),
}

// Extract only the fields needed by the installer
const installerConfig = {
  appId: config.appId,
  productName: config.productName,
  extraMetadata: {
    ...config.extraMetadata,
    installer: installerSpecificConfig,
  },
  copyright: config.copyright,
  artifactName: config.artifactName,
  protocols: config.protocols,
  win: config.win,
  fileAssociations: (config.fileAssociations || []).map((fa) => ({
    ext: fa.ext,
    name: fa.name,
  })),
  directories: config.directories,
}

console.log(JSON.stringify(installerConfig, null, 2))
