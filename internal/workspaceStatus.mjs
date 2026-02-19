import { execSync } from 'child_process'
import fs from 'node:fs'
import { createRequire } from 'node:module'
import path from 'node:path'
import process from 'node:process'
import { fileURLToPath } from 'url'

const scriptDir = path.dirname(fileURLToPath(import.meta.url))
const workspaceRoot = path.resolve(scriptDir, '..')

const require = createRequire(import.meta.url)
const { readDependenciesVersions } = require('./dependenciesVersions.cjs')

// Determine the active environment profile (mode) from NODE_ENV, defaulting to 'production'.
// This determines which .env files are used.
const envProfile = process.env.NODE_ENV || 'production'

// Paths to env files in precedence order (later overrides earlier).
const appGuiDir = path.join(workspaceRoot, 'app', 'gui')
const envFilesInOrder = [
  path.join(appGuiDir, '.env'),
  path.join(appGuiDir, '.env.local'),
  path.join(appGuiDir, `.env.${envProfile}`),
  path.join(appGuiDir, `.env.${envProfile}.local`),
]

/**
 * Parse a simple KEY=VALUE env file into a map.
 * Lines are expected to have no comments, no spaces, no quotes, no interpolation.
 * Missing files yield an empty object.
 * @param {string} filePath path to the env file to parse
 * @returns {StringMap} map of key-value pairs from the env file
 */
function parseEnvFile(filePath) {
  try {
    const content = fs.readFileSync(filePath, 'utf8')
    return content.split(/\r?\n/).reduce((acc, line) => {
      const [key, value] = line.split('=', 2)
      if (key) acc[key] = value
      return acc
    }, {})
  } catch {
    return {}
  }
}

/**
 * Form a default IDE version string from the current date.
 * @returns {string} the version string, like `2025.10.24-dev`
 */
function buildDefaultIdeVersion() {
  const now = new Date()
  const y = now.getFullYear()
  const m = now.getMonth() + 1
  const d = now.getDate()
  return `${y}.${m}.${d}-dev`
}

/**
 * Resolve commit hash using environment variables or git.
 * Priority: ENSO_IDE_COMMIT_HASH env → GITHUB_SHA → `git rev-parse --verify HEAD`.
 * @returns {string} the commit hash, or an empty string if not found
 */
function resolveIdeCommitHash() {
  if (process.env.ENSO_IDE_COMMIT_HASH) return process.env.ENSO_IDE_COMMIT_HASH
  if (process.env.GITHUB_SHA) return process.env.GITHUB_SHA
  try {
    const out = execSync('git rev-parse --verify HEAD', { cwd: workspaceRoot })
    return String(out).trim()
  } catch {
    return ''
  }
}

/**
 * Resolve git ref as current branch, exact tag, or fallback to HEAD.
 * Mirrors BuildInfo.scala behavior.
 * @returns {string} branch or tag name, or `HEAD`
 */
function resolveGitRef() {
  try {
    const out = execSync('git symbolic-ref -q --short HEAD', { cwd: workspaceRoot })
    return String(out).trim()
  } catch {
    try {
      const out = execSync('git describe --tags --exact-match', { cwd: workspaceRoot })
      return String(out).trim()
    } catch {
      return 'HEAD'
    }
  }
}

/**
 * Resolve the latest commit date for HEAD.
 * @returns {string} latest commit date, or an empty string if not found
 */
function resolveGitLatestCommitDate() {
  try {
    const out = execSync('git log HEAD -1 --format=%cd', { cwd: workspaceRoot })
    return String(out).trim()
  } catch {
    return ''
  }
}

// Read env files in order and merge (later files override earlier ones).
const fileMaps = envFilesInOrder.map(parseEnvFile)
let variables = Object.assign({}, ...fileMaps)

// Overlay with actual environment variables when a matching name exists.
for (const key of Object.keys(variables)) {
  const envValue = process.env[key]
  if (envValue !== undefined) variables[key] = envValue
}

// Special handling for version-related variables.
variables['ENSO_IDE_VERSION'] = process.env.ENSO_IDE_VERSION || buildDefaultIdeVersion()
variables['ENSO_IDE_EDITION'] = process.env.ENSO_IDE_EDITION || variables['ENSO_IDE_VERSION']
variables['ENSO_IDE_COMMIT_HASH'] = resolveIdeCommitHash()
variables['ENSO_GIT_REF'] = resolveGitRef()
variables['ENSO_GIT_LATEST_COMMIT_DATE'] = resolveGitLatestCommitDate()

// Project-wide version constants (authoritative source: project/Dependencies.scala).
const deps = readDependenciesVersions({ workspaceRoot })
variables['ENSO_SCALAC_VERSION'] = deps.scalacVersion
variables['ENSO_GRAAL_VERSION'] = deps.graalVersion
variables['ENSO_GRAAL_MAVEN_PACKAGES_VERSION'] = deps.graalMavenPackagesVersion
variables['ENSO_DEFAULT_DEV_VERSION'] = deps.defaultDevEnsoVersion

// Emit all variables to the `stable-status.txt` file.
for (const [key, value] of Object.entries(variables)) {
  console.log(`STABLE_${key} ${value}`)
}
