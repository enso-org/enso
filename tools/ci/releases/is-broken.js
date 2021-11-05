#!/usr/bin/env node
const fs = require('fs')
let usage = `Usage: is-broken.js PATH

Reads the release metadata at PATH (in JSON format) and checks if it contains
the broken mark. Exit code 0 indicates that the release contains the broken
mark. Other exit codes mean that the release either could not be loaded or is
not marked broken. If the release is marked as broken, it also prints the URL to
download the broken mark file.`

if (process.argv.length != 3) {
    console.log(usage)
    process.exit(2)
}

let path = process.argv[2]

function findBrokenMark(release) {
    let assets = release['assets']
    return assets.find(asset => asset['name'] == 'broken')
}

fs.readFile(path, 'utf8', (err, data) => {
    if (err) {
        console.error(err)
        process.exit(2)
    }

    let release = JSON.parse(data)
    let mark = findBrokenMark(release)
    if (mark) {
        console.error('Release is marked as broken.')
        console.log(mark['url'])
        process.exit(0)
    } else {
        console.error('Release is NOT marked as broken.')
        process.exit(1)
    }
})
