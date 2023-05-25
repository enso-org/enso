/** @file Definition of hash computing functions. */

// Eslint is not (and should not be) set up to recognize CommonJS imports.
/* eslint-disable no-restricted-syntax */
const cryptoModule = require('crypto')
const fs = require('fs')
const pathModule = require('path')
/* eslint-enable no-restricted-syntax */

// =================
// === Constants ===
// =================
/** @typedef {"md5" | "sha1" | "sha256"} ChecksumType */
const CHECKSUM_TYPE = 'sha256'

// ================
// === Checksum ===
// ================

/** The `type` argument can be one of `md5`, `sha1`, or `sha256`.
 * @param {string} path - Path to the file.
 * @param {ChecksumType} type - The checksum algorithm to use.
 * @returns {Promise<string>} A promise that resolves to the checksum. */
function getChecksum(path, type) {
    return new Promise(
        // This JSDoc annotation is required for correct types that are also type-safe.
        /** @param {(value: string) => void} resolve - Fulfill the promise with the given value. */
        (resolve, reject) => {
            const hash = cryptoModule.createHash(type)
            const input = fs.createReadStream(path)
            input.on('error', reject)
            input.on('data', chunk => {
                hash.update(chunk)
            })
            input.on('close', () => {
                resolve(hash.digest('hex'))
            })
        }
    )
}

/** Based on https://stackoverflow.com/a/57371333.
 * @param {string} file - The path to the file.
 * @param {string} extension - The new extension of the file.
 * @returns A path with the new exension. */
function changeExtension(file, extension) {
    const basename = pathModule.basename(file, pathModule.extname(file))
    return pathModule.join(pathModule.dirname(file), `${basename}.${extension}`)
}

/** Write the file checksum to the provided path.
 * @param {string} path - The path to the file.
 * @param {ChecksumType} type - The checksum algorithm to use. */
async function writeFileChecksum(path, type) {
    let checksum = await getChecksum(path, type)
    let targetPath = changeExtension(path, type)
    console.log(`Writing ${targetPath}. Checksum is ${checksum}.`)
    await fs.promises.writeFile(targetPath, checksum, 'utf8')
}

// ================
// === Callback ===
// ================

/** Generates checksums for all build artifacts.
 * @param {import('electron-builder').BuildResult} context - Build information. */
exports.default = async function (context) {
    // `context` is BuildResult, see
    // https://www.electron.build/configuration/configuration.html#buildresult
    for (const file of context.artifactPaths) {
        console.log(`Generating ${CHECKSUM_TYPE} checksum for ${file}.`)
        await writeFileChecksum(file, CHECKSUM_TYPE)
    }
    return []
}
