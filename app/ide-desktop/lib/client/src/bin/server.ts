/** @file A simple HTTP server which serves application data to the Electron web-view. */

import * as fs from 'node:fs'
import * as http from 'node:http'
import * as path from 'node:path'

import * as mime from 'mime-types'
import * as portfinder from 'portfinder'
import createServer from 'create-servers'

import * as contentConfig from 'enso-content-config'

import * as paths from '../paths'

const logger = contentConfig.logger

// =================
// === Constants ===
// =================

const HTTP_STATUS_OK = 200

// ==============
// === Config ===
// ==============

/** Constructor parameter for the server configuration. */
interface ConfigConfig {
    dir: string
    port: number
}

/** Server configuration. */
export class Config {
    dir: string
    port: number
    constructor(cfg: ConfigConfig) {
        this.dir = path.resolve(cfg.dir)
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
    server: unknown
    constructor(public config: Config) {}

    /** Server constructor. */
    static async create(config: Config): Promise<Server> {
        const localConfig = Object.assign({}, config)
        localConfig.port = await findPort(localConfig.port)
        const server = new Server(localConfig)
        await server.run()
        return server
    }

    run(): Promise<void> {
        return new Promise((resolve, reject) => {
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
                    logger.log(`Serving files from '${path.join(process.cwd(), this.config.dir)}'.`)
                    resolve()
                }
            )
        })
    }

    process(request: http.IncomingMessage, response: http.ServerResponse) {
        const requestUrl = request.url
        if (requestUrl == null) {
            logger.error('Request URL is null.')
        } else {
            const url = requestUrl.split('?')[0]
            const resource = url === '/' ? '/index.html' : requestUrl
            // `preload.cjs` must be specialcased here as it is loaded by electron from the root,
            // in contrast to all assets loaded by the window, which are loaded from `assets/` via
            // this server.
            const resourceFile =
                resource === '/preload.cjs.map'
                    ? `${paths.APP_PATH}${resource}`
                    : `${this.config.dir}${resource}`
            fs.readFile(resourceFile, (err, data) => {
                if (err) {
                    logger.error(`Resource '${resource}' not found.`)
                } else {
                    const contentType = mime.contentType(path.extname(resourceFile))
                    const contentLength = data.length
                    if (contentType !== false) {
                        response.setHeader('Content-Type', contentType)
                    }
                    response.setHeader('Content-Length', contentLength)
                    response.writeHead(HTTP_STATUS_OK)
                    response.end(data)
                }
            })
        }
    }
}
