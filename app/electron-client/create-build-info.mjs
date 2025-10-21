import fs from 'fs'
import process from 'process'

process.chdir(process.env.JS_BINARY__EXECROOT)

const outputPath = process.argv[2]
if (!outputPath) {
  console.error('Output path is required')
  process.exit(1)
}
const statusFilePath = process.argv[3]
if (!statusFilePath) {
  // This is normal, stamping is disabled. Write a dummy build info file.
  fs.writeFileSync(
    outputPath,
    `export default {
    version: '0.0.0-dev',
    commit: '<snapshot>',
  }`,
    {
      create: true,
    },
  )
  process.exit(0)
}
const statusFile = fs.readFileSync(statusFilePath, 'utf8')

// The file is basically a list of space-separated key-value pairs.
const lines = statusFile.split('\n')
const getValue = (prefix) => lines.find((line) => line.startsWith(prefix)).split(' ')[1]
const ideVersion = getValue('STABLE_IDE_VERSION')
const ideCommitHash = getValue('STABLE_IDE_COMMIT_HASH')

fs.writeFileSync(
  outputPath,
  `export default {
  version: '${ideVersion}',
  commit: '${ideCommitHash}',
}`,
)
