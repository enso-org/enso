/**
 * @file Formats JSON content using prettier.
 *
 * Usage: node prettierJson.mjs <input-file> <output-file>
 *
 * Reads JSON from input file, formats it with prettier, and writes to output file.
 * Uses hardcoded options matching .prettierrc.yaml to avoid loading plugins in Bazel sandbox.
 *
 * The script is used by Bazel to automatically format generated tsconfig files
 * (see bazel_scripts/ts_config.bzl). Running this script is simpler than running prettier
 * executable inside the sandbox and figuring out every input and output of such action.
 * This script operates on single input and has a single output, which is more manageable.
 */

import * as fs from 'node:fs/promises'
import * as prettier from 'prettier'

const [inputFile, outputFile] = process.argv.slice(2)

if (!inputFile || !outputFile) {
  console.error('Usage: node prettierJson.mjs <input-file> <output-file>')
  process.exit(1)
}

const content = await fs.readFile(inputFile, 'utf8')

// Hardcoded options matching .prettierrc.yaml for JSON files
// (plugins aren't needed for JSON and cause issues in Bazel sandbox)
const formatted = await prettier.format(content, {
  parser: 'json',
  tabWidth: 2,
  printWidth: 100,
})

await fs.writeFile(outputFile, formatted)
