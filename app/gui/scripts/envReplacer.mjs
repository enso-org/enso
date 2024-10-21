/* eslint-disable jsdoc/check-tag-names */

// @ts-expect-error missing dotenv typings
import * as dotenv from 'dotenv'
import Buffer from 'node:buffer'
import { createHash } from 'node:crypto'
import * as fs from 'node:fs/promises'
import * as path from 'node:path/posix'
import * as process from 'node:process'

/**
 *
 * @param {string} mode current environment mode
 * @param {string} envDir directory with dotenv file
 * @returns {string[]} list of dotenv file names
 */
function getEnvFilesForMode(mode, envDir) {
  return [
    /** default file */ `.env`,
    /** local file */ `.env.local`,
    /** mode file */ `.env.${mode}`,
    /** mode local file */ `.env.${mode}.local`,
  ].map((file) => path.normalize(path.join(envDir, file)))
}

if (
  process.argv.length !== 5 ||
  process.argv[2] == null ||
  process.argv[3] == null ||
  process.argv[4] == null
)
  throw new Error(
    `Invalid arguments.\nusage:\n  ${process.argv[0]} ${process.argv[1]} <inputDirectory> <outputDirectory> <dotenvDirectory>`,
  )
const inputDirectory = process.argv[2]
const outputDirectory = process.argv[3]
const dotenvDirectory = process.argv[4]

dotenv.config()

const envFiles = getEnvFilesForMode(process.env.NODE_ENV ?? 'development', dotenvDirectory)
const envs = await Promise.all(
  envFiles.map(async (filename) => {
    try {
      const envContents = await fs.readFile(filename, 'utf-8')
      /**
       * @type {Record<string, string>}
       */
      const parsed = dotenv.parse(envContents)
      return parsed
    } catch {
      return {}
    }
  }),
)

const combinedEnvs = Object.assign({}, ...envs)

/**
 * Map of calls mkdir performed so far, to avoid calling it twice on the same path.
 * @type {Map<string, Promise<unknown>>}
 */
const mkdirPromises = new Map()

/**
 * All path renames performed so far, mapping old to new relative project paths.
 * @type {Map<string, string>}
 */
const fileRenames = new Map()

/**
 * Errors found during processing. This process exists with error status when this array is non-empty.
 * @type {Error[]}
 */
const errors = []

async function processAllFiles() {
  const allFiles = await fs.readdir(inputDirectory, { recursive: true, withFileTypes: true })

  /**
   * Thunks of tasks deferred to be completed after visiting all files.
   * @type {Array<() => Promise<unknown>>}
   */
  const deferredThunks = []
  await Promise.all(
    allFiles.map(async (file) => {
      if (!file.isFile() && !file.isSymbolicLink()) return
      const inputPath = path.join(file.path, file.name)
      const projectPath = path.relative(inputDirectory, inputPath)
      if (/config-[0-9a-zA-Z]+\.js$/.test(projectPath)) await processReplacements(projectPath)
      else if (projectPath === 'index.html') deferredThunks.push(() => processIndex(projectPath))
      else {
        var contents = await readOriginalFile(projectPath)
        reportUnexpectedPatterns(contents, projectPath)
        await writeProjectFile(projectPath, contents)
      }
    }),
  )

  await Promise.all(deferredThunks.map((thunk) => thunk()))
}

const patternRegex = /\(\(%__(.*?)__%\)\)/g

/**
 * @param {string} projectPath File path relative to input directory
 */
async function processReplacements(projectPath) {
  const newContent = (await readOriginalFile(projectPath))
    .toString()
    .replace(patternRegex, (pattern, envName) => {
      const envValue = process.env[pattern]
      if (
        Object.prototype.hasOwnProperty.call(process.env, envName) &&
        typeof envValue === 'string'
      ) {
        return envValue
      }
      const envFileValue = combinedEnvs[envName]
      if (
        Object.prototype.hasOwnProperty.call(combinedEnvs, envName) &&
        typeof envFileValue === 'string'
      ) {
        return envFileValue
      } else {
        errors.push(
          new Error(
            `Missing environment variable for replacemnet pattern ${pattern} in file ${projectPath}`,
          ),
        )
        return pattern
      }
    })
  await writeProjectFile(await updateHashInFilename(projectPath, newContent), newContent)
}

/**
 * @param {Buffer} fileContents Contents to inspect for patterns.
 * @param {string} projectPath File path relative to input directory, used for error reporting.
 */
async function reportUnexpectedPatterns(fileContents, projectPath) {
  if (Buffer.isUtf8(fileContents)) {
    for (const match in fileContents.toString('utf8').matchAll(patternRegex)) {
      errors.push(new Error(`Found unexpected pattern ${match} in file "${projectPath}"`))
    }
  }
}

/**
 * Process index file and update the file paths for modified chunks.
 * @param {string} projectPath File path relative to input directory
 */
async function processIndex(projectPath) {
  const newContent = (await readOriginalFile(projectPath))
    .toString()
    // Only attempt to replace simple project-relative paths.
    .replace(/((?:src|href)="\/)([0-9a-zA-Z/.-]+)(")/g, (_, prefix, path, postfix) => {
      const newPath = fileRenames.get(path) ?? path
      if (path != newPath) console.error('Rename in index:', '/' + path, '->', '/' + newPath)
      return prefix + newPath + postfix
    })
  await writeProjectFile(projectPath, newContent)
}

/**
 * @param {string} projectPath  File path relative to input directory
 */
function readOriginalFile(projectPath) {
  return fs.readFile(path.join(inputDirectory, projectPath), { encoding: null })
}

/**
 * Writes given project file to the output directory.
 * @param {string} projectPath File path relative to input directory
 * @param {string | Buffer} fileContents What to write to the new file.
 */
async function writeProjectFile(projectPath, fileContents) {
  const outputPath = path.join(outputDirectory, projectPath)
  const outputDir = path.dirname(outputPath)
  let mkdirPromise = mkdirPromises.get(outputDir)
  if (mkdirPromise == null) {
    mkdirPromise = fs.mkdir(outputDir, { recursive: true })
    mkdirPromises.set(outputDir, mkdirPromise)
  }
  await mkdirPromise
  await fs.writeFile(outputPath, fileContents)
}
/**
 * Recompute file's content hash in case it is present in original filename.
 * Note that this always uses sha256, which may disagree with hasher used during bundle building,
 * therefore calling this function is likely to update the filename even for unchagned files.
 * @param {string} projectPath File path relative to input directory
 * @param {string | Buffer} fileContents Content from which the file hash is computed.
 */
async function updateHashInFilename(projectPath, fileContents) {
  const fixedPath = projectPath.replace(/-([0-9a-zA-Z]+).([a-z]+)$/, (_, oldHash, ext) => {
    const contentHash = createHash('sha256')
      .update(fileContents)
      .digest()
      .toString('base64url')
      .substring(0, oldHash.length)
    return `-${contentHash}.${ext}`
  })
  if (projectPath != fixedPath) fileRenames.set(projectPath, fixedPath)
  return fixedPath
}

await processAllFiles()

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
