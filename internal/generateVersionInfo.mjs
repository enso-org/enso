/**
 * @file Generate `GeneratedVersion.java` which holds build-time versioning information for the Engine.
 */
import { createRequire } from 'node:module'
import path from 'node:path'
import process from 'node:process'
import { readStableStatusFile } from './stableStatus.mjs'

if (process.env.JS_BINARY__EXECROOT) {
  process.chdir(process.env.JS_BINARY__EXECROOT)
}

const require = createRequire(import.meta.url)

function requireStatusVar(vars, key, statusFilePath) {
  const v = vars[key]
  if (!v) {
    throw new Error(`Missing ${key} in ${statusFilePath}`)
  }
  return v
}

function readDependenciesVersionsFromBazelBin() {
  const workspaceRoot = process.env.JS_BINARY__EXECROOT
  if (!workspaceRoot) {
    throw new Error('JS_BINARY__EXECROOT is not set.')
  }

  const bazelBinDir = process.env.BAZEL_BINDIR
  if (!bazelBinDir) {
    throw new Error('BAZEL_BINDIR is not set.')
  }

  const bazelBinRoot = path.resolve(workspaceRoot, bazelBinDir)
  const parserPath = path.join(bazelBinRoot, 'internal', 'dependenciesVersions.cjs')

  try {
    const { readDependenciesVersions } = require(parserPath)
    return readDependenciesVersions({ workspaceRoot: bazelBinRoot })
  } catch (e) {
    throw new Error(`Failed to read dependency versions via ${parserPath}: ${e.message}`)
  }
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

const hasStatusFile = Boolean(statusFilePath)

/** @type {Record<string, string>} */
let stableVars = {}
if (hasStatusFile) {
  stableVars = await readStableStatusFile(statusFilePath)
}

const { scalacVersion, graalVersion, graalMavenPackagesVersion, defaultDevEnsoVersion } =
  readDependenciesVersionsFromBazelBin()

const ensoVersion =
  hasStatusFile ?
    requireStatusVar(stableVars, 'ENSO_IDE_VERSION', statusFilePath)
  : defaultDevEnsoVersion
const currentEdition =
  hasStatusFile ?
    requireStatusVar(stableVars, 'ENSO_IDE_EDITION', statusFilePath)
  : defaultDevEnsoVersion
const commit = stableVars.ENSO_IDE_COMMIT_HASH || '<built outside of a git repository>'
const ref = stableVars.ENSO_GIT_REF || 'HEAD'
const latestCommitDate =
  stableVars.ENSO_GIT_LATEST_COMMIT_DATE || '<built outside of a git repository>'
const isRelease = hasStatusFile

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
    return false;
  }

  static String latestCommitDate() {
    return "${latestCommitDate}";
  }

  static boolean isRelease() {
    return ${isRelease};
  }
}
`.trimStart()

process.stdout.write(fileContents)
