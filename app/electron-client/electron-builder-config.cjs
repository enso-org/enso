module.exports = {
  appId: 'org.enso',
  productName: 'Enso',
  extraMetadata: {
    version: '2025.3.0-dev',
    installer: {},
  },
  artifactName: 'enso-${os}-${arch}-2025.3.0-dev.${ext}',
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
    darkModeSupport: true,
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
    {
      from: 'bundle/',
      to: '.',
    },
    {
      from: '../gui/dist',
      to: 'assets',
    },
  ],
  extraResources: [
    {
      from: '../../built-distribution/enso-engine-0.0.0-dev-${os}-${arch}/enso-0.0.0-dev/',
      to: 'enso/dist/0.0.0-dev',
      filter: ['!THIRD-PARTY{,/**/*}'],
    },
    {
      from: '../../built-small-jdk/',
      to: 'enso/runtime',
    },
  ],
  directories: {
    output: 'ide-dist',
  },
  beforeBuild: function () {
    // We handle node_modules manually.
    return false
  },
  npmRebuild: true,
  buildDependenciesFromSource: false,
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
    // AppImage is known to have sandboxing issues, for example:
    // https://github.com/enso-org/enso/issues/3801 or
    // https://github.com/enso-org/enso/issues/11035
    //
    // A solution to them is to run AppImage with --no-sandbox option (just passing no-sandbox
    // as chrome option didn't seem to work). Wrapped app in a "sandbox fix loader"
    // similar to https://github.com/gergof/electron-builder-sandbox-fix/blob/master/lib/index.js
    // 'electron-builder-sandbox-fix' failed to detect the necessity of sandbox, so we just always
    // add the option instead. This does not lower security, because Enso processes have access
    // to user's filesystem anyway.
    if (context.electronPlatformName !== 'linux') return
    const executableName = context.packager.executableName
    if (!executableName) throw new Error('Expected executableName in context.packager')
    const executable = path.join(context.appOutDir, executableName)
    const loaderScript = `#!/usr/bin/env bash
      set -u

      SCRIPT_DIR="$( cd "$( dirname "\${BASH_SOURCE[0]}" )" && pwd )"
      exec "$SCRIPT_DIR/${executableName}.bin" --no-sandbox "$@"
      `
    try {
      await fs.rename(executable, executable + '.bin')
      await fs.writeFile(executable, loaderScript)
      await fs.chmod(executable, 0o755)
    } catch (e) {
      throw new Error('Failed to create loader for sandbox fix', { cause: e })
    }
  },
}
