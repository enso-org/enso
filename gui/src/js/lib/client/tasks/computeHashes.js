const crypto = require('crypto')
const fs = require('fs')
const glob = require('glob')
const paths = require('../../../../../build/paths')

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

async function writeFileChecksum(path, type) {
    let checksum = await getChecksum(path, type)
    let targetPath = `${path}.${type}`
    fs.writeFile(targetPath, checksum, 'utf8', err => {
        if (err) {
            throw err
        }
    })
}

// ================
// === Callback ===
// ================

exports.default = async function () {
    let files = glob.sync(paths.dist.client + '/*.{dmg,exe,AppImage}')
    for (let file of files) {
        console.log(`Generating ${CHECKSUM_TYPE} checksum for ${file}.`)
        await writeFileChecksum(file, CHECKSUM_TYPE)
    }
    return []
}
