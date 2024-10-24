/** @file A simple HTTP server which serves application data to the Electron web-view. */

import * as mkcert from 'mkcert'
import * as fs from 'node:fs/promises'
import * as http from 'node:http'
import * as path from 'node:path'
import * as stream from 'node:stream'

import createServer from 'create-servers'
import * as mime from 'mime-types'
import * as portfinder from 'portfinder'
import type * as vite from 'vite'

import * as projectManagement from '@/projectManagement'
import * as common from 'enso-common'
import GLOBAL_CONFIG from 'enso-common/src/config.json' assert { type: 'json' }
import * as ydocServer from 'ydoc-server'

import * as contentConfig from '@/contentConfig'
import * as paths from '@/paths'
import { pathToFileURL } from 'node:url'

const logger = contentConfig.logger

ydocServer.configureAllDebugLogs(process.env.ENSO_YDOC_LS_DEBUG === 'true', logger.log.bind(logger))

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
  readonly uploadProjectBundle: (
    project: stream.Readable,
    directory: string | null,
    name: string | null,
  ) => Promise<projectManagement.ProjectInfo>
  readonly runProjectManagerCommand: (
    cliArguments: string[],
    body?: NodeJS.ReadableStream,
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

/**
 * Determine the initial available communication endpoint, starting from the specified port,
 * to provide file hosting services.
 */
async function findPort(port: number): Promise<number> {
  return await portfinder.getPortPromise({ port, startPort: port, stopPort: port + 4 })
}

// ==============
// === Server ===
// ==============

/**
 * A simple server implementation.
 *
 * Initially it was based on `union`, but later we migrated to `create-servers`.
 * Read this topic to learn why: https://github.com/http-party/http-server/issues/483
 */
export class Server {
  projectsRootDirectory: string
  devServer?: vite.ViteDevServer

  /** Create a simple HTTP server. */
  constructor(public config: Config) {
    this.projectsRootDirectory = projectManagement.getProjectsDirectory().replace(/\\/g, '/')
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
  async run(): Promise<void> {
    const defaultValidity = 365
    const ca = await mkcert.createCA({
      organization: 'Enso International Inc.',
      countryCode: 'USA',
      state: 'Delaware',
      locality: 'Wilmington',
      validity: defaultValidity,
    })
    const cert = await mkcert.createCert({
      ca: { key: ca.key, cert: ca.cert },
      domains: ['127.0.0.1', 'localhost'],
      validity: defaultValidity,
    })

    return new Promise((resolve, reject) => {
      createServer(
        {
          https: {
            key: cert.key,
            cert: cert.cert,
            port: this.config.port,
          },
          handler: this.process.bind(this),
        },
        (err, { https: httpsServer, http: httpServer }) => {
          void (async () => {
            if (err) {
              logger.error(`Error creating server:`, err.http)
              reject(err)
            }
            const server = httpsServer ?? httpServer
            if (server) {
              await ydocServer.createGatewayServer(server)
            } else {
              logger.warn('YDocs server is not run, new GUI may not work properly!')
            }
            logger.log(`Server started on port ${this.config.port}.`)
            logger.log(`Serving files from '${path.join(process.cwd(), this.config.dir)}'.`)
            if (process.env.ELECTRON_DEV_MODE === 'true') {
              const vite = (await import(
                pathToFileURL(process.env.NODE_MODULES_PATH + '/vite/dist/node/index.js').href
              )) as typeof import('vite')
              this.devServer = await vite.createServer({
                server: {
                  middlewareMode: true,
                  hmr: server ? { server } : {},
                },
                configFile: process.env.GUI_CONFIG_PATH ?? false,
              })
            }
            resolve()
          })()
        },
      )
    })
  }

  /**
   * Respond to an incoming request.
   * @throws {Error} when passing invalid JSON to
   * `/api/run-project-manager-command?cli-arguments=<urlencoded-json>`.
   */
  process(request: http.IncomingMessage, response: http.ServerResponse) {
    const requestUrl = request.url
    const requestPath = requestUrl?.split('?')[0]?.split('#')[0]
    if (requestUrl == null) {
      logger.error('Request URL is null.')
    } else if (requestUrl.startsWith('/api/project-manager/')) {
      const actualUrl = new URL(
        requestUrl.replace(/^\/api\/project-manager/, GLOBAL_CONFIG.projectManagerHttpEndpoint),
      )
      request.pipe(
        http.request(
          actualUrl,
          { headers: request.headers, method: request.method },
          actualResponse => {
            response.writeHead(
              // This is SAFE. The documentation says:
              // Only valid for response obtained from ClientRequest.
              actualResponse.statusCode!,
              actualResponse.statusMessage,
              actualResponse.headers,
            )
            actualResponse.pipe(response, { end: true })
          },
        ),
        { end: true },
      )
    } else if (request.method === 'POST') {
      switch (requestPath) {
        case '/api/upload-file': {
          const url = new URL(`https://example.com/${requestUrl}`)
          const fileName = url.searchParams.get('file_name')
          const directory = url.searchParams.get('directory') ?? this.projectsRootDirectory
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
                response.writeHead(HTTP_STATUS_BAD_REQUEST, common.COOP_COEP_CORP_HEADERS).end()
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
          const name = url.searchParams.get('name')
          void this.config.externalFunctions
            .uploadProjectBundle(request, directory, name)
            .then(project => {
              response
                .writeHead(HTTP_STATUS_OK, [
                  ['Content-Length', String(project.id.length)],
                  ['Content-Type', 'text/plain'],
                  ...common.COOP_COEP_CORP_HEADERS,
                ])
                .end(project.id)
            })
            .catch(() => {
              response.writeHead(HTTP_STATUS_BAD_REQUEST, common.COOP_COEP_CORP_HEADERS).end()
            })
          break
        }
        case '/api/run-project-manager-command': {
          const cliArguments: unknown = JSON.parse(
            new URL(`https://example.com/${requestUrl}`).searchParams.get('cli-arguments') ?? '[]',
          )
          if (
            !Array.isArray(cliArguments) ||
            !cliArguments.every((item): item is string => typeof item === 'string')
          ) {
            response
              .writeHead(HTTP_STATUS_BAD_REQUEST, common.COOP_COEP_CORP_HEADERS)
              .end('Command arguments must be an array of strings.')
          } else {
            const commandOutput = (() => {
              try {
                return this.config.externalFunctions.runProjectManagerCommand(cliArguments, request)
              } catch {
                const readableStream = new stream.Readable()
                readableStream.push(
                  JSON.stringify({
                    error: `Error running Project Manager command '${JSON.stringify(cliArguments)}'.`,
                  }),
                )
                readableStream.push(null)
                return readableStream
              }
            })()
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
      const url = requestUrl.split('?')[0] ?? ''

      // if it's a path inside the IDE, we need to serve index.html
      const hasExtension = path.extname(url) !== ''

      const resource = hasExtension ? requestUrl : '/index.html'

      // `preload.mjs` must be specialcased here as it is loaded by electron from the root,
      // in contrast to all assets loaded by the window, which are loaded from `assets/` via
      // this server.
      const resourceFile =
        resource === '/preload.mjs.map' ? paths.APP_PATH + resource : this.config.dir + resource
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
