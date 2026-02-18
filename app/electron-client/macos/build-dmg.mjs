#!/usr/bin/env node

import fs from 'node:fs/promises'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

import { parseArgs } from './lib/cli.mjs'
import { getRequiredEnv } from './lib/env.mjs'
import { run } from './lib/exec.mjs'
import { ensurePathExists } from './lib/fs.mjs'
import { createLogger } from './lib/logger.mjs'
import { withPreparedSigningIdentity } from './lib/signing.mjs'
import { notarizeAndStaple } from './notarize-app.mjs'
import { signAppWithIdentity } from './sign-app.mjs'
import { signArchivesWithIdentity } from './sign-archives.mjs'

const SCRIPT_DIR = path.dirname(fileURLToPath(import.meta.url))
const REPO_ROOT = path.resolve(SCRIPT_DIR, '../../..')
const ELECTRON_CLIENT_DIR = path.join(REPO_ROOT, 'app', 'electron-client')
const STAGED_PREPACKAGED_BASE_DIR = path.join(REPO_ROOT, 'dist', 'ide', 'bazel-prepackaged')

function printUsage() {
  console.log(`Usage:
  node app/electron-client/macos/build-dmg.mjs [options]

Options:
  --app <path>          Use an existing app bundle instead of staging from Bazel.
  --entitlements <path> Entitlements plist path.
  --sign                Enable signing (default: disabled).
  --notarize            Enable notarization (default: disabled).
  --verbose             Log executed commands.
  --help                Show this message.

Notes:
  - By default this script performs a dev build: --no-sign --no-notarize.
  - CI should pass --sign --notarize.
`)
}

async function stageAppFromBazel({ verbose, logger }) {
  logger.info('Staging app from Bazel output...')
  run('bazel', ['run', '//app/electron-client:stage_bazel_macos_app'], {
    cwd: REPO_ROOT,
    verbose,
  })
}

async function findStagedApp() {
  const candidates = [
    path.join(STAGED_PREPACKAGED_BASE_DIR, 'mac-arm64', 'Enso.app'),
    path.join(STAGED_PREPACKAGED_BASE_DIR, 'mac', 'Enso.app'),
  ]

  for (const candidate of candidates) {
    try {
      await fs.access(candidate)
      return candidate
    } catch {
      continue
    }
  }

  throw new Error(
    `Unable to locate staged app under ${path.join(STAGED_PREPACKAGED_BASE_DIR, '{mac,mac-arm64}', 'Enso.app')}.`,
  )
}

async function findLatestDmg(distDir) {
  const entries = await fs.readdir(distDir, { withFileTypes: true })
  const dmgs = entries.filter((entry) => entry.isFile() && entry.name.endsWith('.dmg'))
  if (dmgs.length === 0) {
    return undefined
  }

  const withStats = await Promise.all(
    dmgs.map(async (entry) => {
      const fullPath = path.join(distDir, entry.name)
      const stat = await fs.stat(fullPath)
      return { fullPath, mtimeMs: stat.mtimeMs }
    }),
  )

  withStats.sort((left, right) => right.mtimeMs - left.mtimeMs)
  return withStats[0]?.fullPath
}

async function main() {
  if (process.platform !== 'darwin') {
    throw new Error(
      'This script requires macOS because it relies on macOS signing/notarization tools.',
    )
  }

  const parsed = parseArgs(process.argv.slice(2), {
    boolean: ['sign', 'notarize', 'verbose'],
    string: ['app', 'entitlements'],
    defaults: {
      sign: false,
      notarize: false,
      verbose: false,
    },
  })

  if (parsed.help) {
    printUsage()
    return
  }

  if (parsed.notarize && !parsed.sign) {
    throw new Error('--notarize requires --sign because notarization requires a signed app.')
  }

  const logger = createLogger({ verbose: parsed.verbose })

  const entitlementsPath = path.resolve(
    parsed.entitlements ?? path.join(ELECTRON_CLIENT_DIR, 'entitlements.mac.plist'),
  )
  await ensurePathExists(entitlementsPath, 'Entitlements file')

  let appPath
  if (parsed.app) {
    appPath = path.resolve(parsed.app)
    await ensurePathExists(appPath, 'App bundle')
  } else {
    await stageAppFromBazel({ verbose: parsed.verbose, logger })
    appPath = await findStagedApp()
  }

  const prepackagedDir = path.dirname(appPath)
  const ensoVersion = process.env.ENSO_IDE_VERSION?.trim() || '0.0.0-dev'

  if (parsed.sign) {
    const signingEnv = getRequiredEnv(['CSC_LINK', 'CSC_KEY_PASSWORD', 'APPLETEAMID'])

    await withPreparedSigningIdentity({
      env: signingEnv,
      verbose: parsed.verbose,
      run: async ({ identity, keychainPath }) => {
        logger.info('Running archive signing stage...')
        await signArchivesWithIdentity({
          app: appPath,
          entitlements: entitlementsPath,
          identity,
          keychainPath,
          verbose: parsed.verbose,
          logger,
        })

        logger.info('Running app signing stage...')
        await signAppWithIdentity({
          app: appPath,
          entitlements: entitlementsPath,
          identity,
          keychainPath,
          verbose: parsed.verbose,
          logger,
        })
      },
    })
  } else {
    logger.info('Skipping signing stage (default).')
  }

  if (parsed.notarize) {
    const notarizationEnv = getRequiredEnv(['APPLEID', 'APPLEIDPASS', 'APPLETEAMID'])

    logger.info('Running notarization stage...')
    await notarizeAndStaple({
      app: appPath,
      env: notarizationEnv,
      verbose: parsed.verbose,
    })
  } else {
    logger.info('Skipping notarization stage (default).')
  }

  logger.info('Building DMG with electron-builder...')
  run(
    'pnpm',
    [
      '-C',
      'app/electron-client',
      'exec',
      'electron-builder',
      '--mac',
      'dmg',
      '--prepackaged',
      prepackagedDir,
      '--config',
      'macos/electron-builder-dmg-only.cjs',
    ],
    {
      cwd: REPO_ROOT,
      env: {
        ...process.env,
        ENSO_IDE_VERSION: ensoVersion,
      },
      verbose: parsed.verbose,
    },
  )

  const latestDmg = await findLatestDmg(path.join(REPO_ROOT, 'dist', 'ide'))
  if (latestDmg) {
    logger.info(`DMG build completed: ${latestDmg}`)
  } else {
    logger.info('DMG build completed.')
  }
}

const isMainModule =
  process.argv[1] != null && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)

if (isMainModule) {
  main().catch((error) => {
    console.error(error)
    process.exit(1)
  })
}
