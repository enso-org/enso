import * as fs from 'node:fs'

const POSSIBLE_EXEC_PATHS = [
  '../../../dist/ide/linux-unpacked/enso',
  '../../../dist/ide/win-unpacked/Enso.exe',
  '../../../dist/ide/mac-arm64/Enso.app/Contents/MacOS/Enso',
]

/** Setup for all tests: checks if and where electron exec is */
export default function setup() {
  process.env.ENSO_TEST_EXEC_PATH = POSSIBLE_EXEC_PATHS.find(path => {
    try {
      fs.accessSync(path, fs.constants.X_OK)
      return true
    } catch (err) {
      return false
    }
  })
}
