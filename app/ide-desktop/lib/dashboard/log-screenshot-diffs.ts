/** @file Temporary file to print base64 of a png file. */
import * as fs from 'node:fs/promises'

const ROOT = './test-results/'

for (const childName of await fs.readdir(ROOT)) {
    const childPath = ROOT + childName
    for (const fileName of await fs.readdir(childPath)) {
        const filePath = childPath + '/' + fileName
        const file = await fs.readFile(filePath)
        console.log(filePath, file.toString('base64'))
    }
}

process.exit(1)
