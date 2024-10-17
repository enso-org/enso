/** @file Definition of hash computing functions. */

import type { BuildResult } from 'electron-builder'
import * as cryptoModule from 'node:crypto'
import * as fs from 'node:fs'
import * as pathModule from 'node:path'

// =================
// === Constants ===
// =================
const CHECKSUM_TYPE = 'sha256'

// ================
// === Checksum ===
// ================

/**
 * The `type` argument can be one of `md5`, `sha1`, or `sha256`.
 * @param path - Path to the file.
 * @param algorithm - The checksum algorithm to use.
 * @returns A promise that resolves to the checksum.
 */
function getChecksum(path: string, algorithm: string) {
  return new Promise<string>(
    // This JSDoc annotation is required for correct types that are also type-safe.
    /**
     * Promise handler resolving to the file's checksum.
     * @param resolve - Fulfill the promise with the given value.
     */
    (resolve, reject) => {
      const hash = cryptoModule.createHash(algorithm)
      const input = fs.createReadStream(path)
      input.on('error', reject)
      input.on('data', chunk => {
        hash.update(chunk)
      })
      input.on('close', () => {
        resolve(hash.digest('hex'))
      })
    },
  )
}

/**
 * Based on https://stackoverflow.com/a/57371333.
 * @param file - The path to the file.
 * @param extension - The new extension of the file.
 * @returns A path with the new exension.
 */
function changeExtension(file: string, extension: string) {
  const basename = pathModule.basename(file, pathModule.extname(file))
  return pathModule.join(pathModule.dirname(file), `${basename}.${extension}`)
}

/**
 * Write the file checksum to the provided path.
 * @param path - The path to the file.
 * @param algorithm - The checksum algorithm to use.
 */
async function writeFileChecksum(path: string, algorithm: string) {
  const checksum = await getChecksum(path, algorithm)
  const targetPath = changeExtension(path, algorithm)
  console.log(`Writing ${targetPath}. Checksum is ${checksum}.`)
  await fs.promises.writeFile(targetPath, checksum, 'utf8')
}

// ================
// === Callback ===
// ================

/**
 * Generates checksums for all build artifacts.
 * @param context - Build information.
 * @returns afterAllArtifactBuild hook result.
 */
export default async function (context: BuildResult) {
  // `context` is BuildResult, see
  // https://www.electron.build/configuration/configuration.html#buildresult
  for (const file of context.artifactPaths) {
    console.log(`Generating ${CHECKSUM_TYPE} checksum for ${file}.`)
    await writeFileChecksum(file, CHECKSUM_TYPE)
  }
  return []
}
