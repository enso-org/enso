/** @file A simple HTTP server which serves application data to the Electron web-view. */

import * as fs from 'node:fs'
import * as http from 'node:http'
import * as path from 'node:path'
import * as stream from 'node:stream'

import * as mime from 'mime-types'
import * as portfinder from 'portfinder'
import createServer from 'create-servers'

import * as common from 'enso-common'
import * as contentConfig from 'enso-content-config'
import * as ydocServer from 'enso-gui2/ydoc-server'

import * as paths from '../paths'

import GLOBAL_CONFIG from '../../../../../gui/config.yaml' assert { type: 'yaml' }

const logger = contentConfig.logger

// =================
// === Constants ===
// =================

const HTTP_STATUS_OK = 200
const HTTP_STATUS_NOT_FOUND = 404

// ==============
// === Config ===
// ==============

/** External functions for a {@link Server}. */
export interface ExternalFunctions {
    uploadProjectBundle: (project: stream.Readable) => Promise<string>
}

/** Constructor parameter for the server configuration. */
interface ConfigConfig {
    dir: string
    port: number
    externalFunctions: ExternalFunctions
}

/** Server configuration. */
export class Config {
    dir: string
    port: number
    externalFunctions: ExternalFunctions
    /** Create a server configuration. */
    constructor(cfg: ConfigConfig) {
        this.dir = path.resolve(cfg.dir)
        this.port = cfg.port
        this.externalFunctions = cfg.externalFunctions
    }
}

// ===================
// === Port Finder ===
// ===================

/** Determine the initial available communication endpoint, starting from the specified port,
 * to provide file hosting services. */
async function findPort(port: number): Promise<number> {
    return await portfinder.getPortPromise({ port, startPort: port })
}

// ==============
// === Server ===
// ==============

/** A simple server implementation.
 *
 * Initially it was based on `union`, but later we migrated to `create-servers`.
 * Read this topic to learn why: https://github.com/http-party/http-server/issues/483 */
export class Server {
    server: unknown
    /** Create a simple HTTP server. */
    constructor(public config: Config) {}

    /** Server constructor. */
    static async create(config: Config): Promise<Server> {
        const localConfig = Object.assign({}, config)
        localConfig.port = await findPort(localConfig.port)
        const server = new Server(localConfig)
        await server.run()
        return server
    }

    /** Start the server. */
    run(): Promise<void> {
        return new Promise((resolve, reject) => {
            this.server = createServer(
                {
                    http: this.config.port,
                    handler: this.process.bind(this),
                },
                (err, { http: httpServer }) => {
                    if (err) {
                        logger.error(`Error creating server:`, err.http)
                        reject(err)
                    }
                    if (httpServer) {
                        ydocServer.createGatewayServer(httpServer)
                    }
                    logger.log(`Server started on port ${this.config.port}.`)
                    logger.log(`Serving files from '${path.join(process.cwd(), this.config.dir)}'.`)
                    resolve()
                }
            )
        })
    }

    /** Respond to an incoming request. */
    process(request: http.IncomingMessage, response: http.ServerResponse) {
        const requestUrl = request.url
        if (requestUrl == null) {
            logger.error('Request URL is null.')
        } else if (requestUrl.startsWith('/api/project-manager/')) {
            const actualUrl = new URL(
                requestUrl.replace(
                    /^\/api\/project-manager/,
                    GLOBAL_CONFIG.projectManagerHttpEndpoint
                )
            )
            request.pipe(
                http.request(
                    // `...actualUrl` does NOT work because `URL` properties are not enumerable.
                    {
                        headers: request.headers,
                        host: actualUrl.host,
                        hostname: actualUrl.hostname,
                        method: request.method,
                        path: actualUrl.pathname,
                        port: actualUrl.port,
                        protocol: actualUrl.protocol,
                    },
                    actualResponse => {
                        response.writeHead(
                            // This is SAFE. The documentation says:
                            // Only valid for response obtained from ClientRequest.
                            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                            actualResponse.statusCode!,
                            actualResponse.statusMessage,
                            actualResponse.headers
                        )
                        actualResponse.pipe(response, { end: true })
                    }
                ),
                { end: true }
            )
        } else if (request.method === 'POST') {
            const requestPath = requestUrl.split('?')[0]?.split('#')[0]
            switch (requestPath) {
                // This endpoint should only be used when accessing the app from the browser.
                // When accessing the app from Electron, the file input event will have the
                // full system path.
                case '/api/upload-project': {
                    void this.config.externalFunctions.uploadProjectBundle(request).then(id => {
                        response
                            .writeHead(HTTP_STATUS_OK, [
                                ['Content-Length', `${id.length}`],
                                ['Content-Type', 'text/plain'],
                                ...common.COOP_COEP_CORP_HEADERS,
                            ])
                            .end(id)
                    })
                    break
                }
                default: {
                    response.writeHead(HTTP_STATUS_NOT_FOUND, common.COOP_COEP_CORP_HEADERS).end()
                    break
                }
            }
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
            for (const [header, value] of common.COOP_COEP_CORP_HEADERS) {
                response.setHeader(header, value)
            }
            fs.readFile(resourceFile, (err, data) => {
                if (err) {
                    logger.error(`Resource '${resource}' not found.`)
                    response.writeHead(HTTP_STATUS_NOT_FOUND)
                    response.end()
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
