const path = require('node:path')
const fs = require('node:fs/promises')

function artifactName(version) {
  return 'enso-${os}-${arch}-' + version + '.${ext}'
}

function engineDistributionSource(version, platform = process.platform, arch = process.arch) {
  const platformMap = { darwin: 'macos' }
  const archMapByPlatform = {
    darwin: { arm64: 'aarch64' },
    linux: { x64: 'amd64' },
  }

  const normalizedPlatform = platformMap[platform] ?? platform
  const normalizedArch = archMapByPlatform[platform]?.[arch] ?? arch

  return `../../built-distribution/enso-engine-${version}-${normalizedPlatform}-${normalizedArch}/enso-${version}/`
}

function engineDistributionTarget(version) {
  return 'enso/dist/' + version
}

/**
 * electron-builder preserves symlinks in extraResources, but Bazel uses them for sandboxing.
 * This function replaces symlinks with real files by copying their targets.
 *
 * One catch we can face is that this will also replace any expected symlinks in engine distribution.
 * The only symlinks we have at the moment are some legal files in the GraalVM distribution, but they
 * can be safely copied.
 *
 * TODO[ib]: Can we selectively replace only symlinks pointing outside of distribution directory?
 */
async function replaceSymlinksWithFiles(root) {
  const entries = await fs.readdir(root, { withFileTypes: true })
  for (const entry of entries) {
    const full = path.join(root, entry.name)
    const st = await fs.lstat(full)
    if (st.isSymbolicLink()) {
      const linkTarget = await fs.readlink(full)
      const absTarget = path.resolve(path.dirname(full), linkTarget)
      const data = await fs.readFile(absTarget)
      await fs.unlink(full)
      await fs.writeFile(full, data, { mode: st.mode })
    } else if (st.isDirectory()) {
      await replaceSymlinksWithFiles(full)
    }
  }
}

/**
 * AppImage is known to have sandboxing issues, for example:
 * https://github.com/enso-org/enso/issues/3801 or
 * https://github.com/enso-org/enso/issues/11035
 * A solution to them is to run AppImage with --no-sandbox option (just passing no-sandbox
 * as chrome option didn't seem to work). Wrapped app in a "sandbox fix loader"
 * similar to https://github.com/gergof/electron-builder-sandbox-fix/blob/master/lib/index.js
 * 'electron-builder-sandbox-fix' failed to detect the necessity of sandbox, so we just always
 * add the option instead. This does not lower security, because Enso processes have access
 * to user's filesystem anyway.
 */
async function patchAppImage(context) {
  const executableName = context.packager.executableName
  if (!executableName) throw new Error('Expected executableName in context.packager')
  const executable = path.join(context.appOutDir, executableName)
  const loaderScript = `#!/usr/bin/env bash
      set -u

      # TODO[ib]: quick hack to resolve java binary at runtime on Linux
      SCRIPT_DIR="$( cd "$( dirname "\${BASH_SOURCE[0]}" )" && pwd )"
      export PATH="$PATH:$SCRIPT_DIR/resources/enso/runtime/graalvm-ce-java24.0.1-24.2.0/bin"
      exec "$SCRIPT_DIR/${executableName}.bin" --no-sandbox "$@"
      `
  try {
    await fs.rename(executable, executable + '.bin')
    await fs.writeFile(executable, loaderScript)
    await fs.chmod(executable, 0o755)
  } catch (e) {
    throw new Error('Failed to create loader for sandbox fix: ' + e.message)
  }
}

module.exports = {
  appId: 'org.enso',
  productName: 'Enso',
  extraMetadata: {
    version: '0.0.0-dev', // FIXME: Replace with the actual version
    installer: {}, // FIXME: installer config
  },
  copyright: 'Copyright © 2025 New Byte Order sp. z o.o.',
  artifactName: artifactName('0.0.0-dev'), // FIXME: Replace with the actual artifact name
  protocols: [
    {
      name: 'Enso url',
      schemes: ['enso'],
      role: 'Editor',
    },
  ],
  mac: {
    target: 'dir',
    icon: './assets/icons/icon.icns',
    category: 'public.app-category.developer-tools',
    type: 'distribution',
    hardenedRuntime: true,
    gatekeeperAssess: false,
    entitlements: './entitlements.mac.plist',
    entitlementsInherit: './entitlements.mac.plist',
  },
  win: {
    target: 'dir',
    icon: './assets/icons/icon.ico',
  },
  linux: {
    target: 'AppImage',
    icon: './assets/icons/icon.icns',
    category: 'Development',
  },
  files: [
    {
      from: '.',
      to: '.',
      filter: ['package.json', '!**/node_modules/**/*'],
    },
    // TODO: here we duplicate `from` location because stamping and non-stamping builds have different output directories.
    // Can we do it in a more elegant way?
    {
      from: 'bundle/',
      to: '.',
    },
    {
      from: 'bundle_stamped/',
      to: '.',
    },
    {
      from: '../gui/dist',
      to: 'assets',
    },
    {
      from: '../gui/dist_stamped',
      to: 'assets',
    },
  ],
  extraResources: [
    {
      from: engineDistributionSource('0.0.0-dev'),
      to: engineDistributionTarget('0.0.0-dev'),
    },
    {
      from: '../../built-small-jdk/',
      to: 'enso/runtime/graalvm-ce-java24.0.1-24.2.0',
    },
  ],
  fileAssociations: [
    {
      ext: '.enso',
      name: 'Enso Source File',
      role: 'Editor',
      mimeType: 'text/plain',
    },
    {
      ext: '.enso-project',
      name: 'Enso Project Bundle',
      role: 'Editor',
      mimeType: 'application/gzip',
    },
  ],
  directories: {
    output: 'ide-dist',
  },
  // Providing empty beforeBuild hook and using npmRebuild: true to prevent electron-builder from trying to install dependencies.
  beforeBuild: function () {
    return false
  },
  npmRebuild: true,
  nodeGypRebuild: false,
  msi: {
    runAfterFinish: false,
  },
  nsis: {
    differentialPackage: false,
    runAfterFinish: false,
  },
  dmg: {
    writeUpdateInfo: false,
    sign: false,
  },
  publish: null,
  afterPack: async (context) => {
    if (context.electronPlatformName === 'linux') {
      await patchAppImage(context)
    }

    const productName = context.packager.appInfo.productFilename
    const resourcesDir =
      context.electronPlatformName === 'darwin' ?
        path.join(context.appOutDir, `${productName}.app`, 'Contents', 'Resources')
      : path.join(context.appOutDir, 'resources')
    const ensoDir = path.join(resourcesDir, 'enso')
    await replaceSymlinksWithFiles(ensoDir)
  },
}
