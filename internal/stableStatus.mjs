import fs from 'node:fs/promises'

/**
 * Parse Bazel stable-status.txt content into a key/value map.
 * Keys are returned without the leading `STABLE_` prefix.
 *
 * @param {string} content
 * @returns {Record<string, string>}
 */
export function parseStableStatus(content) {
  /** @type {Record<string, string>} */
  const vars = {}
  for (const line of content.split(/\r?\n/)) {
    const separatorIndex = line.indexOf(' ')
    if (separatorIndex === -1) continue
    const key = line.slice(0, separatorIndex)
    const rawValue = line.slice(separatorIndex + 1)
    const value = rawValue.trim().length === 0 ? '' : rawValue
    if (key && key.startsWith('STABLE_ENSO_')) {
      const envName = key.slice('STABLE_'.length)
      vars[envName] = value
    }
  }
  return vars
}

/**
 * Read and parse Bazel stable-status.txt.
 *
 * @param {string} statusFilePath
 * @returns {Promise<Record<string, string>>}
 */
export async function readStableStatusFile(statusFilePath) {
  const content = await fs.readFile(statusFilePath, 'utf8')
  return parseStableStatus(content)
}
