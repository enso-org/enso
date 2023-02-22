/** @file A simple HTTP server which serves application data to the Electron web-view. */

import createServer from 'create-servers'
import * as fs from 'fs'
import * as mime from 'mime-types'
import * as path from 'path'
import * as portfinder from 'portfinder'
import http from 'http'
import { logger } from 'enso-content-config'

// =================
// === Constants ===
// =================

const responses = {
    ok: 200,
}

// ======================
// === URL Parameters ===
// ======================

/** Construct URL query with the given parameters. For each `key` - `value` pair,
 * `key=value` will be added to the query. */
export function urlParamsFromObject(obj: Record<string, string>) {
    const params = []
    for (const [key, value] of Object.entries(obj)) {
        params.push(`${key}=${encodeURIComponent(value)}`)
    }
    return params.length == 0 ? '' : '?' + params.join('&')
}

// ==============
// === Config ===
// ==============

/** Server configuration. */
export class Config {
    dir: string
    port: number
    constructor(cfg: { dir: string; port: number }) {
        this.dir = cfg.dir
        this.port = cfg.port
    }
}

// ===================
// === Port Finder ===
// ===================

/** Determines the initial available communication endpoint, starting from the specified port, to
 * provide file hosting services. */
async function findPort(port: number): Promise<number> {
    return await portfinder.getPortPromise({ port, startPort: port })
}

// ==============
// === Server ===
// ==============

/// A simple server implementation.
///
/// Initially it was based on `union`, but later we migrated to `create-servers`. Read this topic to
/// learn why: https://github.com/http-party/http-server/issues/483
export class Server {
    config: Config
    server: any
    constructor(config: Config) {
        this.config = config
    }

    /** Server constructor. */
    static async create(config: Config): Promise<Server> {
        const local_config = Object.assign({}, config)
        local_config.port = await findPort(local_config.port)
        const server = new Server(local_config)
        await server.run()
        return server
    }

    run(): Promise<void> {
        return new Promise((resolve, reject) => {
            // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
            this.server = createServer(
                {
                    http: this.config.port,
                    handler: this.process.bind(this),
                },
                err => {
                    if (err) {
                        logger.error(`Error creating server:`, err.http)
                        reject(err)
                    }
                    logger.log(`Server started on port ${this.config.port}.`)
                    logger.log(`Serving files from '${process.cwd()}/${this.config.dir}'.`)
                    resolve()
                }
            )
        })
    }

    process(request: http.IncomingMessage, response: http.ServerResponse) {
        const requestUrl = request.url
        if (requestUrl == null) {
            logger.error('Request URL is null.')
            return
        }
        const url = requestUrl.split('?')[0]
        const resource = url == '/' ? '/index.html' : requestUrl
        const resource_file = `${this.config.dir}${resource}`
        fs.readFile(resource_file, (err, data) => {
            if (err) {
                logger.error(`Resource '${resource}' not found.`)
            } else {
                const contentType = mime.contentType(path.extname(resource_file))
                const contentLength = data.length
                if (contentType !== false) {
                    response.setHeader('Content-Type', contentType)
                }
                response.setHeader('Content-Length', contentLength)
                response.writeHead(responses.ok)
                response.end(data)
            }
        })
    }
}
