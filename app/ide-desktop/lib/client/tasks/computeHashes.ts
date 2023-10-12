/** @file Definition of hash computing functions. */

import * as cryptoModule from 'node:crypto'
import * as fs from 'node:fs'
import * as pathModule from 'node:path'

import type * as electronBuilder from 'electron-builder'

// =================
// === Constants ===
// =================

const CHECKSUM_TYPE: ChecksumType = 'sha256'

// ================
// === Checksum ===
// ================

/** All valid checksum algorithms. */
type ChecksumType = 'md5' | 'sha1' | 'sha256'

/** The `type` argument can be one of `md5`, `sha1`, or `sha256`.
 * @param path - Path to the file.
 * @param type - The checksum algorithm to use.
 * @returns A promise that resolves to the checksum. */
function getChecksum(path: string, type: ChecksumType): Promise<string> {
    return new Promise<string>((resolve, reject) => {
        const hash = cryptoModule.createHash(type)
        const input = fs.createReadStream(path)
        input.on('error', reject)
        input.on('data', chunk => {
            hash.update(chunk)
        })
        input.on('close', () => {
            resolve(hash.digest('hex'))
        })
    })
}

/** Based on https://stackoverflow.com/a/57371333.
 * @param file - The path to the file.
 * @param extension - The new extension of the file.
 * @returns A path with the new exension. */
function changeExtension(file: string, extension: string): string {
    const basename = pathModule.basename(file, pathModule.extname(file))
    return pathModule.join(pathModule.dirname(file), `${basename}.${extension}`)
}

/** Write the file checksum to the provided path.
 * @param path - The path to the file.
 * @param type - The checksum algorithm to use. */
async function writeFileChecksum(path: string, type: ChecksumType): Promise<void> {
    const checksum = await getChecksum(path, type)
    const targetPath = changeExtension(path, type)
    console.log(`Writing ${targetPath}. Checksum is ${checksum}.`)
    await fs.promises.writeFile(targetPath, checksum, 'utf8')
}

// ================
// === Callback ===
// ================

/** Generates checksums for all build artifacts.
 * @param context - Build information.
 * @returns `afterAllArtifactBuild` hook result. */
export default async function (context: electronBuilder.BuildResult): Promise<string[]> {
    for (const file of context.artifactPaths) {
        console.log(`Generating ${CHECKSUM_TYPE} checksum for ${file}.`)
        await writeFileChecksum(file, CHECKSUM_TYPE)
    }
    return []
}
