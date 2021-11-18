// This module implements a simple http server that listens for crash reports as POST requests and
// writes them to disk. To start the server, call `startServer(port)` or execute this file through
// Node.js. More information can be foun in the README.md.

const express = require('express')
const fs = require('fs')
const uuid = require('uuid')
const yargs = require('yargs')

const defaultPort = require('../../config.js').defaultLogServerPort

module.exports = {
    startServer,
}

function main(argv) {
    startServer(parse_args(argv).port)
}

// =================
// === Constants ===
// =================

const httpStatusCodes = {
    noContent: 204,
    forbidden: 403,
    badRequest: 400,
    internalServerError: 500,
}

// ========================
// === Argument Parsing ===
// ========================

function parse_args(argv) {
    return yargs(argv)
        .option('port', {
            alias: 'p',
            description:
                'The number of the port that this server will listen on. ' +
                'If the the number is 0 then an arbitrary free port will be chosen.',
            type: 'number',
            default: defaultPort,
        })
        .help()
        .alias('help', 'h').argv
}

// ==============
// === Server ===
// ==============

function startServer(port) {
    const app = express()
    app.use(express.text())

    app.post('/', async (req, res) => {
        if (
            typeof req.headers.origin === 'undefined' ||
            new URL(req.headers.origin).hostname !== 'localhost'
        ) {
            res.sendStatus(httpStatusCodes.forbidden)
        } else if (typeof req.body !== 'string') {
            res.sendStatus(httpStatusCodes.badRequest)
        } else {
            try {
                await writeLog(req.body)
                console.log(`Saved log from origin ${req.headers.origin}`)
                res.sendStatus(httpStatusCodes.noContent)
            } catch (e) {
                console.error('Could not write log file:\n' + e.message)
                res.sendStatus(httpStatusCodes.internalServerError)
            }
        }
    })

    const server = app.listen(port, 'localhost')
    server.on('listening', function () {
        console.log(`Logging service listening at port ${server.address().port}`)
    })
    return server
}

// ==================
// === File Utils ===
// ==================

/**
 * Writes message to a new file in the log sub directory.
 * The file name is composed of the UTC time and date and a V4 UUID to guarantee uniqueness.
 */
async function writeLog(message) {
    const dir = 'log'
    const file = `${timestamp()}__${uuid.v4()}`
    await fs.promises.mkdir(dir, { recursive: true })
    await fs.promises.writeFile(`${dir}/${file}`, message)
}

/**
 * Returns the current UTC date and time in the format "yyy-MM-dd_HH:mm:ss.".
 */
function timestamp() {
    const d = new Date()

    const year = d.getUTCFullYear().toString()
    const month = d.getUTCMonth().toString().padStart(2, '0')
    const day = d.getUTCDate().toString().padStart(2, '0')

    const hour = d.getUTCHours().toString().padStart(2, '0')
    const minute = d.getUTCMinutes().toString().padStart(2, '0')
    const second = d.getUTCSeconds().toString().padStart(2, '0')

    return `${year}-${month}-${day}_${hour}:${minute}:${second}`
}

if (require.main === module) {
    const command_line_args = process.argv.slice(2)
    main(command_line_args)
}
