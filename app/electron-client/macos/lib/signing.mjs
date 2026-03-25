import { getRequiredEnv } from './env.mjs'
import {
  cleanupSigningKeychain,
  resolveSigningIdentity,
  setupSigningKeychain,
} from './keychain.mjs'
import { installCleanupHandlers } from './signal.mjs'

export function buildCodesignArgs({
  target,
  identity,
  entitlements,
  keychainPath,
  verbose = false,
}) {
  const args = ['--entitlements', entitlements, '--force', '--options=runtime', '--timestamp']
  if (verbose) {
    args.unshift('-vvv')
  }
  args.push('--sign', identity, '--keychain', keychainPath, target)
  return args
}

export async function withPreparedSigningIdentity({ env, verbose = false, run }) {
  const signingEnv = env ?? getRequiredEnv(['CSC_LINK', 'CSC_KEY_PASSWORD', 'APPLETEAMID'])

  let signingKeychainState
  const uninstallHandlers = installCleanupHandlers(async () => {
    await cleanupSigningKeychain(signingKeychainState, { verbose })
  })

  try {
    signingKeychainState = await setupSigningKeychain({
      cscLink: signingEnv.CSC_LINK,
      cscKeyPassword: signingEnv.CSC_KEY_PASSWORD,
      verbose,
    })

    const identity = resolveSigningIdentity({
      appleteamid: signingEnv.APPLETEAMID,
      keychainPath: signingKeychainState.keychainPath,
      verbose,
    })

    return await run({ identity, keychainPath: signingKeychainState.keychainPath })
  } finally {
    uninstallHandlers()
    await cleanupSigningKeychain(signingKeychainState, { verbose })
  }
}
