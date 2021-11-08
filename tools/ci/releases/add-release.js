#!/usr/bin/env node
const fs = require('fs')
let usage = `Usage: add-release.js PATH TAG [ASSETS...]

Updates the release list at PATH by adding a new release (if it does not exist)
with the provided TAG and list of ASSETS.`

if (process.argv.length < 4) {
    console.log(usage)
    process.exit(2)
}

let path = process.argv[2]
let tag = process.argv[3]
let assets = process.argv.slice(4)

if (assets.length == 0) {
    console.error('Adding a release with no assets.')
}

function releaseAlreadyExists(root, tag) {
    let existing = root['releases'].find(release => release['tag'] == tag)
    return existing !== undefined
}

fs.readFile(path, 'utf8', (err, data) => {
    if (err) {
        console.error(err)
        process.exit(2)
    }

    let root = JSON.parse(data)
    if (releaseAlreadyExists(root, tag)) {
        console.error(`Release '${tag}' already exists.`)
        console.error('No changes written.')
        process.exit(1)
    }

    let release = {
        tag: tag,
        assets: assets,
    }

    root['releases'].push(release)

    fs.writeFile(path, JSON.stringify(root, null, 1) + '\n', err => {
        if (err) {
            console.error(err)
            process.exit(2)
        } else {
            console.error(`Added release ${tag} with assets ${assets}.`)
        }
    })
})
