import fs from 'node:fs/promises'
import os from 'node:os'
import path from 'node:path'

export async function ensurePathExists(target, label) {
  try {
    await fs.access(target)
  } catch {
    throw new Error(`${label} does not exist: ${target}`)
  }
}

export async function rmRf(targetPath) {
  await fs.rm(targetPath, { recursive: true, force: true })
}

export async function getTmpDir(prefix = 'enso-signing-') {
  return await fs.mkdtemp(path.join(os.tmpdir(), prefix))
}
