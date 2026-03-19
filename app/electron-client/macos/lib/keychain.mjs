import crypto from 'node:crypto'
import fs from 'node:fs/promises'
import path from 'node:path'

import { run } from './exec.mjs'
import { getTmpDir, rmRf } from './fs.mjs'
import { createLogger } from './logger.mjs'

function listUserKeychains({ verbose = false } = {}) {
  const output = run('security', ['list-keychains', '-d', 'user'], { verbose, stdio: 'pipe' })
  return output
    .split('\n')
    .map((line) => line.trim())
    .filter((line) => line.length > 0)
    .map((line) => line.replace(/^"|"$/g, ''))
}

function setUserKeychainSearchList(keychains, { verbose = false } = {}) {
  if (!Array.isArray(keychains) || keychains.length === 0) {
    return
  }
  run('security', ['list-keychains', '-d', 'user', '-s', ...keychains], { verbose })
}

export async function setupSigningKeychain({ cscLink, cscKeyPassword, verbose = false }) {
  const workingDir = await getTmpDir('enso-signing-keychain-')
  const keychainName = `enso-signing-${crypto.randomUUID()}.keychain-db`
  const keychainPath = path.join(workingDir, keychainName)
  const keychainPassword = crypto.randomBytes(24).toString('base64')
  const p12Path = path.join(workingDir, 'cert.p12')
  const existingKeychains = listUserKeychains({ verbose })

  const normalizedBase64 = cscLink.replace(/\s+/g, '')
  if (normalizedBase64.length === 0) {
    throw new Error('CSC_LINK is empty after removing whitespace.')
  }

  const p12Buffer = Buffer.from(normalizedBase64, 'base64')
  if (p12Buffer.length === 0) {
    throw new Error('CSC_LINK does not decode to a non-empty PKCS#12 payload.')
  }
  await fs.writeFile(p12Path, p12Buffer)

  run('security', ['create-keychain', '-p', keychainPassword, keychainPath], {
    redactValues: [keychainPassword],
    verbose,
  })
  run('security', ['set-keychain-settings', '-lut', '21600', keychainPath], { verbose })
  run('security', ['unlock-keychain', '-p', keychainPassword, keychainPath], {
    redactValues: [keychainPassword],
    verbose,
  })
  run(
    'security',
    [
      'import',
      p12Path,
      '-k',
      keychainPath,
      '-P',
      cscKeyPassword,
      '-T',
      '/usr/bin/codesign',
      '-T',
      '/usr/bin/security',
    ],
    {
      redactValues: [cscKeyPassword],
      verbose,
    },
  )
  run(
    'security',
    [
      'set-key-partition-list',
      '-S',
      'apple-tool:,apple:',
      '-s',
      '-k',
      keychainPassword,
      keychainPath,
    ],
    {
      redactValues: [keychainPassword],
      verbose,
    },
  )

  const mergedKeychains = [
    keychainPath,
    ...existingKeychains.filter((entry) => entry !== keychainPath),
  ]
  setUserKeychainSearchList(mergedKeychains, { verbose })

  return { keychainPath, workingDir, existingKeychains }
}

export async function cleanupSigningKeychain(state, { verbose = false } = {}) {
  if (!state) {
    return
  }

  const logger = createLogger({ verbose })

  try {
    if (Array.isArray(state.existingKeychains) && state.existingKeychains.length > 0) {
      setUserKeychainSearchList(state.existingKeychains, { verbose })
    }
  } catch (error) {
    logger.warn(`Failed restoring keychain search list: ${error}`)
  }

  try {
    run('security', ['delete-keychain', state.keychainPath], { verbose })
  } catch (error) {
    logger.warn(`Failed deleting temporary keychain: ${error}`)
  }

  await rmRf(state.workingDir)
}

export function listCodesigningIdentities(keychainPath, { verbose = false } = {}) {
  const output = run('security', ['find-identity', '-v', '-p', 'codesigning', keychainPath], {
    verbose,
    stdio: 'pipe',
  })
  return output
    .split('\n')
    .map((line) => {
      const match = line.match(/\)\s+([0-9A-F]{40})\s+"([^"]+)"/i)
      if (!match) {
        return undefined
      }
      const hash = match[1]
      const name = match[2]
      if (!hash || !name) {
        return undefined
      }
      return { hash, name }
    })
    .filter((value) => value != null)
}

export function resolveSigningIdentity({ appleteamid, keychainPath, verbose = false }) {
  const logger = createLogger({ verbose })
  const identities = listCodesigningIdentities(keychainPath, { verbose })
  if (identities.length === 0) {
    throw new Error(`No codesigning identities were found in temporary keychain ${keychainPath}.`)
  }

  const identityNames = identities.map((identity) => identity.name)

  const expectedIdentity = `Developer ID Application: Enso International Inc. (${appleteamid})`
  const expected = identities.find((identity) => identity.name === expectedIdentity)
  if (expected) {
    return expected.hash
  }

  const fallback = identities.find((identity) =>
    identity.name.startsWith('Developer ID Application: '),
  )
  if (fallback) {
    logger.warn(
      `Expected identity '${expectedIdentity}' not found. Using '${fallback.name}' from imported keychain.`,
    )
    return fallback.hash
  }

  throw new Error(
    `No 'Developer ID Application' identity found in imported keychain. Available identities: ${identityNames.join(', ')}`,
  )
}
