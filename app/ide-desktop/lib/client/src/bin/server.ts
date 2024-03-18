/** @file A simple HTTP server which serves application data to the Electron web-view. */

import * as fs from 'node:fs/promises'
import * as fsSync from 'node:fs'
import * as http from 'node:http'
import * as os from 'node:os'
import * as path from 'node:path'
import type * as stream from 'node:stream'

import * as mime from 'mime-types'
import * as portfinder from 'portfinder'
import type * as vite from 'vite'
import createServer from 'create-servers'

import * as common from 'enso-common'
import * as contentConfig from 'enso-content-config'
import * as ydocServer from 'enso-gui2/ydoc-server'

import * as paths from '../paths'

// prettier-ignore
import GLOBAL_CONFIG from '../../../../../gui2/config.yaml' assert { type: 'yaml' }

const logger = contentConfig.logger

// =================
// === Constants ===
// =================

const HTTP_STATUS_OK = 200
const HTTP_STATUS_BAD_REQUEST = 400
const HTTP_STATUS_NOT_FOUND = 404

// ==============
// === Config ===
// ==============

/** External functions for a {@link Server}. */
export interface ExternalFunctions {
    readonly uploadProjectBundle: (project: stream.Readable, directory?: string) => Promise<string>
    readonly runProjectManagerCommand: (
        cliArguments: string[],
        body?: NodeJS.ReadableStream
    ) => NodeJS.ReadableStream
}

/** Constructor parameter for the server configuration. */
interface ConfigConfig {
    readonly dir: string
    readonly port: number
    readonly externalFunctions: ExternalFunctions
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
    projectsRootDirectory: string
    devServer?: vite.ViteDevServer

    /** Create a simple HTTP server. */
    constructor(public config: Config) {
        this.projectsRootDirectory = path.join(os.homedir(), 'enso/projects')
    }

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
            createServer(
                {
                    http: this.config.port,
                    handler: this.process.bind(this),
                },
                (err, { http: httpServer }) => {
                    void (async () => {
                        if (err) {
                            logger.error(`Error creating server:`, err.http)
                            reject(err)
                        }
                        // Prepare the YDoc server access point for the new Vue-based GUI.
                        // TODO[ao]: This is very ugly quickfix to make our rust-ffi WASM
                        // working both in browser and in ydocs server. Doing it properly
                        // is tracked in https://github.com/enso-org/enso/issues/8931
                        const assets = path.join(paths.ASSETS_PATH, 'assets')
                        const bundledFiles = fsSync.existsSync(assets)
                            ? await fs.readdir(assets)
                            : []
                        const rustFFIWasm = bundledFiles.find(name =>
                            /rust_ffi_bg-.*\.wasm/.test(name)
                        )
                        if (httpServer && rustFFIWasm != null) {
                            await ydocServer.createGatewayServer(
                                httpServer,
                                path.join(assets, rustFFIWasm)
                            )
                        } else {
                            logger.warn('YDocs server is not run, new GUI may not work properly!')
                        }
                        logger.log(`Server started on port ${this.config.port}.`)
                        logger.log(
                            `Serving files from '${path.join(process.cwd(), this.config.dir)}'.`
                        )
                        resolve()
                        if (process.env.ELECTRON_DEV_MODE === 'true') {
                            // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/consistent-type-imports, @typescript-eslint/no-var-requires
                            const vite = require('vite') as typeof import('vite')
                            void vite
                                .createServer({
                                    server: {
                                        middlewareMode: true,
                                        hmr: httpServer ? { server: httpServer } : {},
                                    },
                                    configFile: process.env.GUI_CONFIG_PATH ?? false,
                                })
                                .then(devServer => (this.devServer = devServer))
                        }
                    })()
                }
            )
        })
    }

    /** Respond to an incoming request.
     * @throws {Error} when passing invalid JSON to
     * `/api/run-project-manager-command?cli-arguments=<urlencoded-json>`. */
    process(request: http.IncomingMessage, response: http.ServerResponse) {
        const requestUrl = request.url
        const requestPath = requestUrl?.split('?')[0]?.split('#')[0]
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
            switch (requestPath) {
                case '/api/upload-file': {
                    const url = new URL(`https://example.com/${requestUrl}`)
                    const fileName = url.searchParams.get('file_name')
                    const directory =
                        url.searchParams.get('directory') ?? this.projectsRootDirectory
                    if (fileName == null) {
                        response
                            .writeHead(HTTP_STATUS_BAD_REQUEST, common.COOP_COEP_CORP_HEADERS)
                            .end('Request is missing search parameter `file_name`.')
                    } else {
                        const filePath = path.join(directory, fileName)
                        void fs
                            .writeFile(filePath, request)
                            .then(() => {
                                response
                                    .writeHead(HTTP_STATUS_OK, [
                                        ['Content-Length', String(filePath.length)],
                                        ['Content-Type', 'text/plain'],
                                        ...common.COOP_COEP_CORP_HEADERS,
                                    ])
                                    .end(filePath)
                            })
                            .catch(e => {
                                console.error(e)
                                response
                                    .writeHead(
                                        HTTP_STATUS_BAD_REQUEST,
                                        common.COOP_COEP_CORP_HEADERS
                                    )
                                    .end()
                            })
                    }
                    break
                }
                // This endpoint should only be used when accessing the app from the browser.
                // When accessing the app from Electron, the file input event will have the
                // full system path.
                case '/api/upload-project': {
                    const url = new URL(`https://example.com/${requestUrl}`)
                    const directory = url.searchParams.get('directory')
                    const directoryParams = directory == null ? [] : [directory]
                    void this.config.externalFunctions
                        .uploadProjectBundle(request, ...directoryParams)
                        .then(id => {
                            response
                                .writeHead(HTTP_STATUS_OK, [
                                    ['Content-Length', String(id.length)],
                                    ['Content-Type', 'text/plain'],
                                    ...common.COOP_COEP_CORP_HEADERS,
                                ])
                                .end(id)
                        })
                        .catch(() => {
                            response
                                .writeHead(HTTP_STATUS_BAD_REQUEST, common.COOP_COEP_CORP_HEADERS)
                                .end()
                        })
                    break
                }
                case '/api/run-project-manager-command': {
                    const cliArguments: unknown = JSON.parse(
                        new URL(`https://example.com/${requestUrl}`).searchParams.get(
                            'cli-arguments'
                        ) ?? '[]'
                    )
                    if (
                        !Array.isArray(cliArguments) ||
                        !cliArguments.every((item): item is string => typeof item === 'string')
                    ) {
                        response
                            .writeHead(HTTP_STATUS_BAD_REQUEST, common.COOP_COEP_CORP_HEADERS)
                            .end('Command arguments must be an array of strings.')
                    } else {
                        const commandOutput =
                            this.config.externalFunctions.runProjectManagerCommand(
                                cliArguments,
                                request
                            )
                        response.writeHead(HTTP_STATUS_OK, [
                            ['Content-Type', 'application/json'],
                            ...common.COOP_COEP_CORP_HEADERS,
                        ])
                        commandOutput.pipe(response, { end: true })
                    }
                    break
                }
                default: {
                    response.writeHead(HTTP_STATUS_NOT_FOUND, common.COOP_COEP_CORP_HEADERS).end()
                    break
                }
            }
        } else if (request.method === 'GET' && requestPath === '/api/root-directory') {
            response
                .writeHead(HTTP_STATUS_OK, [
                    ['Content-Length', String(this.projectsRootDirectory.length)],
                    ['Content-Type', 'text/plain'],
                    ...common.COOP_COEP_CORP_HEADERS,
                ])
                .end(this.projectsRootDirectory)
        } else if (this.devServer) {
            this.devServer.middlewares(request, response)
        } else {
            const url = requestUrl.split('?')[0]
            const resource = url === '/' ? '/index.html' : requestUrl
            // `preload.cjs` must be specialcased here as it is loaded by electron from the root,
            // in contrast to all assets loaded by the window, which are loaded from `assets/` via
            // this server.
            const resourceFile =
                resource === '/preload.cjs.map'
                    ? paths.APP_PATH + resource
                    : this.config.dir + resource
            for (const [header, value] of common.COOP_COEP_CORP_HEADERS) {
                response.setHeader(header, value)
            }
            fs.readFile(resourceFile)
                .then(data => {
                    const contentType = mime.contentType(path.extname(resourceFile))
                    const contentLength = data.length
                    if (contentType !== false) {
                        response.setHeader('Content-Type', contentType)
                    }
                    response.setHeader('Content-Length', contentLength)
                    response.writeHead(HTTP_STATUS_OK)
                    response.end(data)
                })
                .catch(() => {
                    logger.error(`Resource '${resource}' not found.`)
                    response.writeHead(HTTP_STATUS_NOT_FOUND)
                    response.end()
                })
        }
    }
}
