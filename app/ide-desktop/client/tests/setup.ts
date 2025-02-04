/** @file {@link setup} function for all tests. */

import * as fs from 'node:fs'
import * as path from 'node:path'
import { fileURLToPath } from 'node:url'

const __dirname = path.dirname(fileURLToPath(import.meta.url))

const POSSIBLE_EXEC_PATHS = [
  path.resolve(__dirname, '../../../../dist/ide/linux-unpacked/enso'),
  path.resolve(__dirname, '../../../../dist/ide/win-unpacked/Enso.exe'),
  path.resolve(__dirname, '../../../../dist/ide/mac/Enso.app/Contents/MacOS/Enso'),
  path.resolve(__dirname, '../../../../dist/ide/mac-arm64/Enso.app/Contents/MacOS/Enso'),
]

/**
 * Setup for all tests: checks if and where electron exec is.
 * @throws when no Enso package could be found.
 */
export default function setup() {
  const execPath = POSSIBLE_EXEC_PATHS.find((p) => {
    try {
      fs.accessSync(p, fs.constants.X_OK)
      return true
    } catch {
      return false
    }
  })
  if (execPath != null) {
    process.env.ENSO_TEST_EXEC_PATH = execPath
  } else {
    throw Error('Cannot find Enso package')
  }
}
