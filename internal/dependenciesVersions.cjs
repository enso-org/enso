const fs = require('node:fs')
const path = require('node:path')

/**
 * @typedef {Object} DependenciesVersions
 * @property {string} scalacVersion
 * @property {string} graalVersion
 * @property {string} graalMavenPackagesVersion
 * @property {string} defaultDevEnsoVersion
 */

/**
 * Extract a string constant from `project/Dependencies.scala`.
 *
 * Supported declaration forms:
 * - `val name = "..."`
 * - `lazy val name = "..."`
 * - either of the above with an optional type annotation before `=`
 *
 * The parser is intentionally strict and fails fast when a required value is
 * missing, so Bazel/Node build tooling cannot silently diverge from SBT values.
 *
 * @param {string} contents Full source of `project/Dependencies.scala`.
 * @param {string} name Scala constant name to extract.
 * @param {string} filePath Absolute path used in error messages.
 */
function parseScalaValString(contents, name, filePath) {
  const re = new RegExp(
    String.raw`^\s*(?:lazy\s+)?val\s+${name}(?:\s*:\s*[^=]+)?\s*=\s*"([^"]+)"`,
    'm',
  )
  const match = contents.match(re)
  if (!match || !match[1]) {
    throw new Error(
      `Failed to parse \"${name}\" from ${filePath}. Expected a line like: val ${name} = \"...\"`,
    )
  }
  return match[1]
}

/**
 * Read project-wide version constants from `project/Dependencies.scala`.
 *
 * This module is used by JS-based build scripts that need the same authoritative
 * values as Scala build definitions.
 *
 * @param {{ workspaceRoot: string }} params
 * @returns {DependenciesVersions}
 */
function readDependenciesVersions({ workspaceRoot }) {
  if (!workspaceRoot || typeof workspaceRoot !== 'string') {
    throw new Error(
      `readDependenciesVersions: expected { workspaceRoot: string }, got: ${workspaceRoot}`,
    )
  }

  const filePath = path.join(workspaceRoot, 'project', 'Dependencies.scala')
  let contents
  try {
    contents = fs.readFileSync(filePath, 'utf8')
  } catch (e) {
    throw new Error(`Failed to read ${filePath}: ${e.message}`)
  }

  const scalacVersion = parseScalaValString(contents, 'scalacVersion', filePath)
  const graalVersion = parseScalaValString(contents, 'graalVersion', filePath)
  const graalMavenPackagesVersion = parseScalaValString(
    contents,
    'graalMavenPackagesVersion',
    filePath,
  )
  const defaultDevEnsoVersion = parseScalaValString(contents, 'defaultDevEnsoVersion', filePath)

  return {
    scalacVersion,
    graalVersion,
    graalMavenPackagesVersion,
    defaultDevEnsoVersion,
  }
}

module.exports = { readDependenciesVersions }
