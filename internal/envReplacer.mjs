/**
 * @file A bundle post-processing script used by bazel's `stamp_files` target.
 *
 * In order to aid cache usage and ensure that the testing and production bundles are as
 * similar as possible, we place placeholder environment variables in code.
 * Afterwards, this script is used to replace those placeholders in compiled
 * assets, so that the appropriate environment configuration can be applied without any
 * source code changes.
 *
 * If stamping is disabled, the script just copies files to the output path.
 *
 * Otherwise, we read `stable-status.txt` file produced by `workspaceStatus` script,
 * and use the environment variables from it to replace the placeholders in the files.
 *
 * For bundles that use hashed output filenames (for example GUI assets), the script can also
 * recalculate filename hashes and recursively rewrite references to renamed files.
 *
 * Finally, we perform validation to ensure that no placeholders are present in unexpected locations.
 */

/* eslint-disable jsdoc/check-tag-names */

import Buffer from 'node:buffer'
import { createHash } from 'node:crypto'
import * as fs from 'node:fs/promises'
import * as path from 'node:path/posix'
import * as process from 'node:process'
import { readStableStatusFile } from './stableStatus.mjs'

if (process.env.JS_BINARY__EXECROOT) {
  process.chdir(process.env.JS_BINARY__EXECROOT)
}

if (
  process.argv.length < 5 ||
  process.argv[2] == null ||
  process.argv[3] == null ||
  process.argv[4] == null
)
  throw new Error(
    `Invalid arguments.\nusage:\n  ${process.argv[0]} ${process.argv[1]} <inputPath> <outputPath> <recalculateHashes:true|false> [statusFilePath]`,
  )
const inputPath = process.argv[2]
const outputPath = process.argv[3]
const recalculateHashesArg = process.argv[4]
const statusFilePath = process.argv[5]

/**
 * @param {string} value
 */
function parseBooleanArg(value) {
  if (value === 'true') return true
  if (value === 'false') return false
  throw new Error(`Invalid boolean argument: "${value}". Expected "true" or "false".`)
}

const recalculateHashes = parseBooleanArg(recalculateHashesArg)

/**
 * Match files that should have environment variable replacements applied.
 * Covers:
 * - config.js or config-<hash>.js (GUI config files)
 * - index.html (GUI entry point)
 * - index.mjs or preload.mjs (Electron client entry points)
 * - electron-builder-config.cjs (Electron Builder config)
 *
 * @param {string} projectPath
 */
function isEnvReplacementFile(projectPath) {
  // Matches config.js and config-<hash>.js.
  const configFileRegex = /^config(?:-[0-9A-Za-z]+)?\.js$/

  const base = path.basename(projectPath)
  if (base === 'index.html') return true
  if (base === 'index.mjs') return true
  if (base === 'preload.mjs') return true
  if (base === 'electron-builder-config.cjs') return true
  if (configFileRegex.test(base)) return true
  return false
}

/**
 * Map of calls mkdir performed so far, to avoid calling it twice on the same path.
 * @type {Map<string, Promise<unknown>>}
 */
const mkdirPromises = new Map()

/**
 * Track filename (basename) renames only: old basename -> new basename.
 * @type {Map<string, string>}
 */
const fileRenames = new Map()

/**
 * Errors found during processing. This process exists with error status when this array is non-empty.
 * @type {Error[]}
 */
const errors = []

function assertNoErrors() {
  if (errors.length > 0) {
    console.error('==========================')
    console.error(
      `Found ${errors.length} error${errors.length > 1 ? 's' : ''} during bundle postprocessing:`,
    )
    console.error('')
    for (const error of errors) {
      console.error(error)
      console.error('')
    }
    process.exit(1)
  }
}

/** @type {Record<string, string>} */
let envs = {}

// When stamping, the status file contains environment variables to replace.
// We only consider variables starting with `ENSO_` prefix, and we only consider `stable-status.txt` file,
// so each variable is expected to start with `STABLE_ENSO_` prefix.
if (statusFilePath != null) {
  try {
    envs = await readStableStatusFile(statusFilePath)
  } catch (e) {
    errors.push(new Error(`Failed to read status file "${statusFilePath}": ${e.message}`))
  }
}

let inputStats
try {
  inputStats = await fs.stat(inputPath)
} catch (e) {
  errors.push(new Error(`Failed to stat input path "${inputPath}": ${e.message}`))
  assertNoErrors()
}

const inputIsDirectory = inputStats.isDirectory()
const inputDirectory = inputIsDirectory ? inputPath : null
const outputDirectory = inputIsDirectory ? outputPath : null

const patternRegex = /\(\(%__(.*?)__%\)\)/g

/**
 * @param {string} content File content to apply replacements to.
 * @param {string} projectPath File path relative to input directory, used for error reporting only.
 */
function applyReplacements(content, projectPath) {
  return content.replace(patternRegex, (pattern, envName) => {
    const envFileValue = envs[envName]
    if (envFileValue != null && typeof envFileValue === 'string') {
      return envFileValue
    } else {
      errors.push(
        new Error(
          `Missing environment variable for replacement pattern ${pattern} in file ${projectPath}`,
        ),
      )
      return pattern
    }
  })
}

/**
 * @param {Buffer} fileContents Contents to inspect for patterns.
 * @param {string} projectPath File path relative to input directory, used for error reporting.
 */
async function reportUnexpectedPatterns(fileContents, projectPath) {
  if (Buffer.isUtf8(fileContents)) {
    for (const match of fileContents.toString().matchAll(patternRegex)) {
      errors.push(new Error(`Found unexpected pattern ${match} in file "${projectPath}"`))
    }
  }
}

/**
 * @param {string} projectPath  File path relative to input directory
 */
function readOriginalFile(projectPath) {
  return fs.readFile(path.join(inputDirectory, projectPath), { encoding: null })
}

/**
 * Writes file to the output path, creating parent directory when needed.
 * @param {string} filePath Absolute or execroot-relative output file path.
 * @param {string | Buffer} fileContents What to write to the output file.
 */
async function writeFileEnsuringDirectory(filePath, fileContents) {
  const outputDir = path.dirname(filePath)
  let mkdirPromise = mkdirPromises.get(outputDir)
  if (mkdirPromise == null) {
    mkdirPromise = fs.mkdir(outputDir, { recursive: true })
    mkdirPromises.set(outputDir, mkdirPromise)
  }
  await mkdirPromise
  await fs.writeFile(filePath, fileContents)
}

function readOutputFile(projectPath) {
  return fs.readFile(path.join(outputDirectory, projectPath), { encoding: null })
}

async function deleteOutputFile(projectPath) {
  try {
    await fs.unlink(path.join(outputDirectory, projectPath))
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
  } catch (e) {
    // Ignore errors.
  }
}
/**
 * Recompute file's content hash in case it is present in original filename.
 * Note that this always uses sha256, which may disagree with hasher used during bundle building,
 * therefore calling this function is likely to update the filename even for unchanged files.
 * The behavior is controlled by the `recalculateHashes` argument.
 * @param {string} projectPath File path relative to input directory
 * @param {string | Buffer} fileContents Content from which the file hash is computed.
 */
async function updateHashInFilename(projectPath, fileContents) {
  if (!recalculateHashes) return projectPath

  const base = path.basename(projectPath)
  const dir = path.dirname(projectPath)

  const match = base.match(/^(.*)-([0-9A-Za-z]{8,})(\.[^./]+(?:\.[^./]+)*)$/)
  if (!match || !match[1] || !match[2] || !match[3]) return projectPath

  const namePrefix = match[1]
  const oldHash = match[2]
  const extension = match[3]
  const contentHash = createHash('sha256')
    .update(fileContents)
    .digest()
    .toString('base64url')
    .substring(0, oldHash.length)
  const newBase = `${namePrefix}-${contentHash}${extension}`
  fileRenames.set(base, newBase)
  return path.join(dir, newBase)
}

// Enumerate files under a directory (recursively) returning project-relative paths.
async function enumerateFiles(rootDir) {
  const entries = await fs.readdir(rootDir, { recursive: true, withFileTypes: true })
  const files = []
  for (const file of entries) {
    if (!file.isFile() && !file.isSymbolicLink()) continue
    const fullPath = path.join(file.parentPath.replace(/\\/g, '/'), file.name)
    const rel = path.relative(rootDir, fullPath)
    files.push(rel)
  }
  return files
}

/**
 * Rewrite references inside quoted strings by swapping basenames.
 * @param {string} content File content to rewrite references in.
 * @returns {{ content: string, changed: boolean }} An object with the rewritten content and a boolean indicating if any changes were made.
 */
function rewriteReferencesInText(content) {
  if (fileRenames.size === 0) return { content, changed: false }
  let changed = false
  const rewritten = content.replace(/(["'])([^"']+)\1/g, (m, quote, inner) => {
    let updated = inner
    for (const [oldBase, newBase] of fileRenames.entries()) {
      if (updated.includes(oldBase)) updated = updated.replaceAll(oldBase, newBase)
    }
    if (updated !== inner) {
      changed = true
      return quote + updated + quote
    } else {
      return m
    }
  })
  return { content: rewritten, changed }
}

/**
 * Initial pass: copy from input to output, applying replacements for matching files.
 */
async function initialPassWriteToOutput(outputDir) {
  const inputFiles = await enumerateFiles(inputDirectory)
  await Promise.all(
    inputFiles.map(async (projectPath) => {
      const buf = await readOriginalFile(projectPath)
      const isText = Buffer.isUtf8(buf)
      if (statusFilePath != null && isText && isEnvReplacementFile(projectPath)) {
        const newContent = applyReplacements(buf.toString(), projectPath)
        const newPath = await updateHashInFilename(projectPath, newContent)
        await writeFileEnsuringDirectory(path.join(outputDir, newPath), newContent)
      } else {
        await writeFileEnsuringDirectory(path.join(outputDir, projectPath), buf)
      }
    }),
  )
}

/**
 * Single-file flow: copy one file, optionally apply replacements, and validate.
 */
async function processSingleFile() {
  const fileContents = await fs.readFile(inputPath, { encoding: null })
  const projectPath = path.basename(inputPath)
  const isText = Buffer.isUtf8(fileContents)

  if (statusFilePath != null && isText && isEnvReplacementFile(projectPath)) {
    await writeFileEnsuringDirectory(
      outputPath,
      applyReplacements(fileContents.toString(), projectPath),
    )
  } else {
    await writeFileEnsuringDirectory(outputPath, fileContents)
  }

  const writtenContents = await fs.readFile(outputPath, { encoding: null })
  if (Buffer.isUtf8(writtenContents) && !isEnvReplacementFile(projectPath)) {
    await reportUnexpectedPatterns(writtenContents, projectPath)
  }
}

/**
 * One cascade pass over the output directory: rewrite references and rename changed files.
 * @returns {Promise<number>} The number of changed files.
 */
async function cascadePassOnce(outputDir) {
  const outFiles = await enumerateFiles(outputDirectory)
  let changedCount = 0
  for (const projectPath of outFiles) {
    const buf = await readOutputFile(projectPath)
    if (!Buffer.isUtf8(buf)) continue
    const { content, changed } = rewriteReferencesInText(buf.toString())
    if (changed) {
      const newPath = await updateHashInFilename(projectPath, content)
      if (newPath !== projectPath) {
        await deleteOutputFile(projectPath)
      }
      await writeFileEnsuringDirectory(path.join(outputDir, newPath), content)
      changedCount++
    }
  }
  return changedCount
}

/**
 * Final validation sweep over the output directory.
 * We check that no placeholders are present in unexpected locations.
 */
async function finalValidationSweep() {
  const outFiles = await enumerateFiles(outputDirectory)
  await Promise.all(
    outFiles.map(async (projectPath) => {
      const buf = await readOutputFile(projectPath)
      if (!Buffer.isUtf8(buf)) return
      if (!isEnvReplacementFile(projectPath)) {
        await reportUnexpectedPatterns(buf, projectPath)
      }
    }),
  )
}

if (inputIsDirectory) {
  // Execute directory flow with cascading rewrites.
  await initialPassWriteToOutput(outputDirectory)

  if (statusFilePath != null && recalculateHashes) {
    let passes = 0
    while (passes < 10) {
      const changed = await cascadePassOnce(outputDirectory)
      if (changed === 0) break
      passes++
    }
  }

  await finalValidationSweep()
} else {
  await processSingleFile()
}

assertNoErrors()
process.exit(0)
