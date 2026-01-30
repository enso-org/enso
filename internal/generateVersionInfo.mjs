/**
 * @file Generate `GeneratedVersion.java` which holds build-time versioning information for the Engine.
 */
import fs from 'node:fs/promises'
import process from 'node:process'

if (process.env.JS_BINARY__EXECROOT) {
  process.chdir(process.env.JS_BINARY__EXECROOT)
}

/**
 * Parse Bazel stable-status.txt into a key/value map.
 * Keys are returned without the leading 'STABLE_' prefix.
 *
 * @param {string} content
 * @returns {Record<string, string>}
 */
function parseStableStatus(content) {
  /** @type {Record<string, string>} */
  const vars = {}
  for (const line of content.split(/\r?\n/)) {
    const [key, value] = line.split(' ', 2)
    if (key && key.startsWith('STABLE_ENSO')) {
      const envName = key.slice('STABLE_'.length)
      vars[envName] = value
    }
  }
  return vars
}

function requireStatusVar(vars, key, statusFilePath) {
  const v = vars[key]
  if (!v) {
    throw new Error(`Missing ${key} in ${statusFilePath}`)
  }
  return v
}

function requireEnvVar(vars, key) {
  const v = vars[key]
  if (!v) {
    throw new Error(`Missing required environment variable ${key}`)
  }
  return v
}

let statusFilePath
let idx = 2
while (idx < process.argv.length) {
  const arg = process.argv[idx]
  if (arg === '--status-file') {
    statusFilePath = process.argv[idx + 1]
    idx += 2
    continue
  }
  throw new Error(`Unknown argument ${arg}`)
}

if (!statusFilePath) {
  statusFilePath = process.env.BAZEL_STABLE_STATUS_FILE
}
if (!statusFilePath) {
  throw new Error(
    'Missing required status-file.txt path. Provide --status-file or build with stamping enabled.',
  )
}

const content = await fs.readFile(statusFilePath, 'utf8')
const stableVars = parseStableStatus(content)

const scalacVersion = requireEnvVar(process.env, 'ENSO_SCALAC_VERSION')
const graalVersion = requireEnvVar(process.env, 'ENSO_GRAAL_VERSION')
const graalMavenPackagesVersion = requireEnvVar(process.env, 'ENSO_GRAAL_MAVEN_PACKAGES_VERSION')
const defaultDevEnsoVersion = requireEnvVar(process.env, 'ENSO_DEFAULT_DEV_VERSION')
const ensoVersion = requireStatusVar(stableVars, 'ENSO_IDE_VERSION', statusFilePath)
const currentEdition = requireStatusVar(stableVars, 'ENSO_IDE_EDITION', statusFilePath)
const commit = stableVars.ENSO_IDE_COMMIT_HASH || '<built outside of a git repository>'
const ref = stableVars.ENSO_GIT_REF || 'HEAD'
const isDirty = stableVars.ENSO_GIT_IS_DIRTY === 'true' ? 'true' : 'false'
const latestCommitDate =
  stableVars.ENSO_GIT_LATEST_COMMIT_DATE || '<built outside of a git repository>'

const fileContents = `
package org.enso.version;

final class GeneratedVersion {
  private GeneratedVersion() {}

  static String defaultDevEnsoVersion() {
    return "${defaultDevEnsoVersion}";
  }

  static String ensoVersion() {
    return "${ensoVersion}";
  }

  static String scalacVersion() {
    return "${scalacVersion}";
  }

  static String graalVersion() {
    return "${graalMavenPackagesVersion}";
  }

  static String javaVersion() {
    return "${graalVersion}";
  }

  static String currentEdition() {
    return "${currentEdition}";
  }

  static String commit() {
    return "${commit}";
  }

  static String ref() {
    return "${ref}";
  }

  static boolean isDirty() {
    return ${isDirty};
  }

  static String latestCommitDate() {
    return "${latestCommitDate}";
  }

  static boolean isRelease() {
    return true;
  }
}
`.trimStart()

process.stdout.write(fileContents)
