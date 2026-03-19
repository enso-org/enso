const path = require('node:path')

const DEFAULT_VERSION = '0.0.0-dev'

function resolveVersion() {
  const version = process.env.ENSO_IDE_VERSION
  if (typeof version !== 'string' || version.trim().length === 0) {
    return DEFAULT_VERSION
  }
  return version.trim()
}

const ensoVersion = resolveVersion()

module.exports = {
  appId: 'org.enso',
  productName: 'Enso',
  extraMetadata: {
    version: ensoVersion,
  },
  artifactName: 'enso-${os}-${arch}-${version}.${ext}',
  directories: {
    output: path.resolve(__dirname, '../../../dist/ide'),
  },
  mac: {
    target: 'dmg',
  },
  beforeBuild: function () {
    return false
  },
  npmRebuild: true,
  nodeGypRebuild: false,
  dmg: {
    writeUpdateInfo: false,
    sign: false,
    icon: path.resolve(__dirname, '../assets/icons/icon.icns'),
  },
  publish: null,
}
