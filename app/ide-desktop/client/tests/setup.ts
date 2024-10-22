/** @file {@link setup} function for all tests. */

import * as fs from 'node:fs'

const POSSIBLE_EXEC_PATHS = [
  '../../../dist/ide/linux-unpacked/enso',
  '../../../dist/ide/win-unpacked/Enso.exe',
  '../../../dist/ide/mac/Enso.app/Contents/MacOS/Enso',
  '../../../dist/ide/mac-arm64/Enso.app/Contents/MacOS/Enso',
]

/**
 * Setup for all tests: checks if and where electron exec is.
 * @throws when no Enso package could be found.
 */
export default function setup() {
  const execPath = POSSIBLE_EXEC_PATHS.find(path => {
    try {
      fs.accessSync(path, fs.constants.X_OK)
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
