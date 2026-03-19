import path from 'node:path'

import { run } from './lib/exec.mjs'
import { ensurePathExists } from './lib/fs.mjs'
import { globAbsoluteIn } from './lib/glob.mjs'
import { createLogger } from './lib/logger.mjs'
import { buildCodesignArgs } from './lib/signing.mjs'

async function collectNestedSignables(appDir) {
  const patterns = [
    'Contents/Frameworks/**/*.app',
    'Contents/Frameworks/**/*.xpc',
    'Contents/Frameworks/**/*.framework',
    'Contents/Frameworks/**/*.{dylib,so,node}',
    'Contents/Frameworks/**/Helpers/chrome_crashpad_handler',
    'Contents/Frameworks/**/Resources/ShipIt',
    'Contents/PlugIns/**/*.appex',
    'Contents/PlugIns/**/*.xpc',
    'Contents/Library/LoginItems/**/*.app',
  ]

  const matches = await globAbsoluteIn(appDir, patterns, {
    dot: true,
    onlyFiles: false,
    unique: true,
  })

  return matches
    .map((entry) => path.resolve(entry))
    .sort((left, right) => right.split(path.sep).length - left.split(path.sep).length)
}

function signPath({ target, identity, entitlements, keychainPath, verbose }) {
  const args = buildCodesignArgs({ target, identity, entitlements, keychainPath, verbose })
  run('codesign', args, { verbose })
}

export async function signAppWithIdentity({
  app,
  entitlements,
  identity,
  keychainPath,
  verbose = false,
  logger,
}) {
  await ensurePathExists(app, 'App bundle')
  await ensurePathExists(entitlements, 'Entitlements file')

  const resolvedLogger = logger ?? createLogger({ verbose })

  const appDir = path.resolve(app)
  run('chmod', ['-R', 'u+w', appDir], { verbose })

  resolvedLogger.info('Signing nested app components...')
  const nestedSignables = await collectNestedSignables(appDir)
  for (const signablePath of nestedSignables) {
    signPath({
      target: signablePath,
      identity,
      entitlements,
      keychainPath,
      verbose,
    })
  }

  resolvedLogger.info('Signing app bundle...')
  signPath({
    target: appDir,
    identity,
    entitlements,
    keychainPath,
    verbose,
  })

  resolvedLogger.info('Verifying signature...')
  const verifyArgs = ['--verify', '--deep', '--strict']
  if (verbose) {
    verifyArgs.push('--verbose=2')
  }
  verifyArgs.push(appDir)
  run('codesign', verifyArgs, { verbose })

  resolvedLogger.info('App signing completed successfully.')
}
