import fs from 'node:fs/promises'
import os from 'node:os'
import path from 'node:path'

import { getRequiredEnv } from './lib/env.mjs'
import { run } from './lib/exec.mjs'
import { ensurePathExists } from './lib/fs.mjs'
import { createLogger } from './lib/logger.mjs'

const SUBMITTABLE_EXTENSIONS = new Set(['.zip', '.pkg', '.dmg'])

function isAppBundle(target) {
  return target.toLowerCase().endsWith('.app')
}

async function prepareSubmissionPath(target, { verbose = false } = {}) {
  if (!isAppBundle(target)) {
    const extension = path.extname(target).toLowerCase()
    if (!SUBMITTABLE_EXTENSIONS.has(extension)) {
      throw new Error(
        `Unsupported input format: ${target}. Expected .app, .zip, .pkg, or .dmg for notarization.`,
      )
    }
    return { submissionPath: target }
  }

  const temporaryDir = await fs.mkdtemp(path.join(os.tmpdir(), 'enso-notarize-'))
  const archivePath = path.join(temporaryDir, `${path.basename(target)}.zip`)
  run('ditto', ['-c', '-k', '--sequesterRsrc', '--keepParent', target, archivePath], { verbose })
  return { submissionPath: archivePath, temporaryDir }
}

function parseJsonOutput(text, context) {
  try {
    return JSON.parse(text)
  } catch {
    throw new Error(`Failed to parse ${context} JSON output: ${text}`)
  }
}

function fetchNotarizationLog({ id, env, verbose }) {
  const logOutput = run(
    'xcrun',
    [
      'notarytool',
      'log',
      id,
      '--output-format',
      'json',
      '--apple-id',
      env.APPLEID,
      '--team-id',
      env.APPLETEAMID,
      '--password',
      env.APPLEIDPASS,
    ],
    {
      redactValues: [env.APPLEIDPASS],
      verbose,
      stdio: 'pipe',
    },
  )
  return parseJsonOutput(logOutput, `notarytool log (${id})`)
}

function ensureAcceptedSubmission({ submitResult, env, verbose, logger }) {
  if (submitResult?.status === 'Accepted') {
    logger.info(`Notarization accepted (id: ${submitResult.id}).`)
    return
  }

  const id = submitResult?.id
  if (typeof id === 'string' && id.length > 0) {
    const notarizationLog = fetchNotarizationLog({ id, env, verbose })
    logger.error('Notarization submission failed. Detailed log:')
    logger.error(JSON.stringify(notarizationLog, null, 2))
  }

  throw new Error(
    `Notarization did not succeed. Status: ${submitResult?.status ?? 'unknown'}${id ? ` (id: ${id})` : ''}`,
  )
}

export async function notarizeAndStaple({ app, env, verbose = false }) {
  await ensurePathExists(app, 'App bundle')

  const logger = createLogger({ verbose })
  const notarizationEnv = env ?? getRequiredEnv(['APPLEID', 'APPLEIDPASS', 'APPLETEAMID'])
  const prepared = await prepareSubmissionPath(app, { verbose })

  try {
    logger.info('Submitting app for notarization...')
    const submitOutput = run(
      'xcrun',
      [
        'notarytool',
        'submit',
        prepared.submissionPath,
        '--wait',
        '--output-format',
        'json',
        '--apple-id',
        notarizationEnv.APPLEID,
        '--team-id',
        notarizationEnv.APPLETEAMID,
        '--password',
        notarizationEnv.APPLEIDPASS,
      ],
      {
        redactValues: [notarizationEnv.APPLEIDPASS],
        verbose,
        stdio: 'pipe',
      },
    )

    const submitResult = parseJsonOutput(submitOutput, 'notarytool submit')
    ensureAcceptedSubmission({ submitResult, env: notarizationEnv, verbose, logger })
  } finally {
    if (prepared.temporaryDir) {
      await fs.rm(prepared.temporaryDir, { recursive: true, force: true })
    }
  }

  logger.info('Stapling notarization ticket...')
  run('xcrun', ['stapler', 'staple', '-v', app], { verbose })
  run('xcrun', ['stapler', 'validate', '-v', app], { verbose })

  logger.info('App notarization and stapling completed successfully.')
}
