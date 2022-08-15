const crypto = require('crypto')
const fs = require('fs')
const path = require('path')

// =================
// === Constants ===
// =================

const CHECKSUM_TYPE = 'sha256'

// ================
// === Checksum ===
// ================

/// The `type` argument can be one of `md5`, `sha1`, or `sha256`.
function getChecksum(path, type) {
    return new Promise(function (resolve, reject) {
        const hash = crypto.createHash(type)
        const input = fs.createReadStream(path)
        input.on('error', reject)
        input.on('data', function (chunk) {
            hash.update(chunk)
        })
        input.on('close', function () {
            resolve(hash.digest('hex'))
        })
    })
}

// Based on https://stackoverflow.com/a/57371333
function changeExtension(file, extension) {
    const basename = path.basename(file, path.extname(file))
    return path.join(path.dirname(file), `${basename}.${extension}`)
}

async function writeFileChecksum(path, type) {
    let checksum = await getChecksum(path, type)
    let targetPath = changeExtension(path, type)
    console.log(`Writing ${targetPath}.`)
    fs.writeFile(targetPath, checksum, 'utf8', err => {
        if (err) {
            throw err
        }
    })
}

// ================
// === Callback ===
// ================

exports.default = async function (context) {
    // `context` is BuildResult, see https://www.electron.build/configuration/configuration.html#buildresult
    for (let file of context.artifactPaths) {
        console.log(`Generating ${CHECKSUM_TYPE} checksum for ${file}.`)
        await writeFileChecksum(file, CHECKSUM_TYPE)
    }
    return []
}
