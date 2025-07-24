/** @file A simple HTTP server which serves application data to the Electron web-view. */

import * as mkcert from 'mkcert'
import * as fs from 'node:fs/promises'
import * as http from 'node:http'
import * as https from 'node:https'
import * as path from 'node:path'
import * as stream from 'node:stream'

import createServer from 'create-servers'
import * as mime from 'mime-types'
import * as portfinder from 'portfinder'
import type * as vite from 'vite'
import * as yaml from 'yaml'

import * as projectManagement from '@/projectManagement'
import { COOP_COEP_CORP_HEADERS } from 'enso-common'
import GLOBAL_CONFIG from 'enso-common/src/config.json' with { type: 'json' }
import * as ydocServer from 'ydoc-server'

import * as contentConfig from '@/contentConfig'
import * as paths from '@/paths'
import { pathToFileURL } from 'node:url'

const logger = contentConfig.logger

ydocServer.configureAllDebugLogs(
  process.env.ENSO_IDE_YDOC_LS_DEBUG === 'true',
  logger.log.bind(logger),
)

// =================
// === Constants ===
// =================

const HTTP_STATUS_OK = 200
const HTTP_STATUS_BAD_REQUEST = 400
const HTTP_STATUS_NOT_FOUND = 404
const HTTP_STATUS_INTERNAL_SERVER_ERROR = 500
const IS_ELECTRON_DEV_MODE = process.env.ELECTRON_DEV_MODE === 'true'

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
            if (!IS_ELECTRON_DEV_MODE) {
              if (server) {
                await ydocServer.createGatewayServer(server)
              } else {
                logger.warn('YDocs server is not run, new GUI may not work properly!')
              }
            }
            logger.log(`Server started on port ${this.config.port}.`)
            logger.log(`Serving files from '${path.resolve(process.cwd(), this.config.dir)}'.`)
            if (IS_ELECTRON_DEV_MODE) {
              const vite = (await import(
                pathToFileURL(process.env.NODE_MODULES_PATH + '/vite/dist/node/index.js').href
              )) as typeof import('vite')
              this.devServer = await vite.createServer({
                server: {
                  middlewareMode: true,
                  hmr: server ? { server } : {},
                },
                configFile: process.env.GUI_CONFIG_PATH ?? false,
                mode: process.env.MODE ?? 'staging',
              })

              const docServer = http.createServer()
              docServer.on('request', (request, response) => {
                if (request.method === 'GET' && request.url === '/_health') {
                  response.writeHead(200, { 'Content-Type': 'text/plain; charset=UTF-8' }).end('OK')
                }
              })

              await ydocServer.createGatewayServer(docServer)

              docServer.listen(5976, 'localhost', () => {
                console.log(`Ydoc server listening on localhost:5976`)
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
          (actualResponse) => {
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
    } else if (requestUrl.startsWith('/api/cloud/')) {
      switch (requestPath) {
        case '/api/cloud/download-project': {
          const url = new URL(`https://example.com/${requestUrl}`)
          const downloadUrl = url.searchParams.get('downloadUrl')
          const projectId = url.searchParams.get('projectId')

          if (downloadUrl == null) {
            response
              .writeHead(HTTP_STATUS_BAD_REQUEST, COOP_COEP_CORP_HEADERS)
              .end('Request is missing search parameter `downloadUrl`.')
            break
          }

          if (projectId == null) {
            response
              .writeHead(HTTP_STATUS_BAD_REQUEST, COOP_COEP_CORP_HEADERS)
              .end('Request is missing search parameter `projectId`.')
            break
          }

          https.get(downloadUrl, async (actualResponse) => {
            const projectsDirectory = projectManagement.getProjectsDirectory()
            const parentDirectory = path.join(projectsDirectory, `cloud-${projectId}`)
            const projectRootDirectory = path.join(parentDirectory, 'project_root')

            try {
              await fs.mkdir(projectRootDirectory, { recursive: true })
              await projectManagement.unpackBundle(actualResponse, projectRootDirectory)
              response
                .writeHead(HTTP_STATUS_OK, COOP_COEP_CORP_HEADERS)
                .end(JSON.stringify({ projectRootDirectory, parentDirectory }))
            } catch (e) {
              logger.error(e)
              await fs
                .access(parentDirectory)
                .then(() => {
                  fs.rmdir(parentDirectory, { maxRetries: 3, recursive: true })
                })
                .catch((e) => {
                  logger.error(`Failed to cleanup directory ${parentDirectory}.`, e)
                })
              response.writeHead(HTTP_STATUS_INTERNAL_SERVER_ERROR, COOP_COEP_CORP_HEADERS).end()
            }
          })

          break
        }
        case '/api/cloud/get-project-archive': {
          const url = new URL(`https://example.com/${requestUrl}`)
          const parentDir = url.searchParams.get('directory')

          if (parentDir == null) {
            response
              .writeHead(HTTP_STATUS_BAD_REQUEST, COOP_COEP_CORP_HEADERS)
              .end('Request is missing search parameter `directory`.')
            break
          }
          const projectDir = path.join(parentDir, 'project_root')

          projectManagement
            .createBundle(projectDir)
            .then((projectBundle) => {
              response
                .writeHead(HTTP_STATUS_OK, {
                  ...COOP_COEP_CORP_HEADERS,
                  'Content-Length': String(projectBundle.byteLength),
                })
                .end(projectBundle)
            })
            .catch((err) => {
              logger.error(err)
              response.writeHead(HTTP_STATUS_INTERNAL_SERVER_ERROR, COOP_COEP_CORP_HEADERS).end()
            })

          break
        }
        default: {
          logger.error(`Unknown Cloud middleware request:`, requestPath)
          break
        }
      }
    } else if (request.method === 'POST') {
      switch (requestPath) {
        case '/api/directories': {
          break
        }
        case '/api/directories/search': {
          break
        }
        case '/api/upload-file': {
          const url = new URL(`https://example.com/${requestUrl}`)
          const fileName = url.searchParams.get('file_name')
          const directory = url.searchParams.get('directory') ?? this.projectsRootDirectory
          if (fileName == null) {
            response
              .writeHead(HTTP_STATUS_BAD_REQUEST, COOP_COEP_CORP_HEADERS)
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
                    ...COOP_COEP_CORP_HEADERS,
                  ])
                  .end(filePath)
              })
              .catch((e) => {
                console.error(e)
                response.writeHead(HTTP_STATUS_BAD_REQUEST, COOP_COEP_CORP_HEADERS).end()
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
            .then((project) => {
              response
                .writeHead(HTTP_STATUS_OK, [
                  ['Content-Length', String(project.id.length)],
                  ['Content-Type', 'text/plain'],
                  ...COOP_COEP_CORP_HEADERS,
                ])
                .end(project.id)
            })
            .catch(() => {
              response.writeHead(HTTP_STATUS_BAD_REQUEST, COOP_COEP_CORP_HEADERS).end()
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
              .writeHead(HTTP_STATUS_BAD_REQUEST, COOP_COEP_CORP_HEADERS)
              .end('Command arguments must be an array of strings.')
          } else if (
            (cliArguments[0] === '--filesystem-list' ||
              cliArguments[0] === '--filesystem-list-recursive') &&
            cliArguments[1] != null
          ) {
            void apiPmListDirectory(
              cliArguments[1],
              cliArguments[0] === '--filesystem-list-recursive',
            ).then(
              (entries) => {
                response.writeHead(HTTP_STATUS_OK, [
                  ['Content-Type', 'application/json'],
                  ...COOP_COEP_CORP_HEADERS,
                ])
                response.end(JSON.stringify({ jsonrpc: '2.0', id: 0, result: { entries } }))
              },
              () => {
                response.writeHead(HTTP_STATUS_OK, COOP_COEP_CORP_HEADERS)
                response.end(
                  JSON.stringify({
                    jsonrpc: '2.0',
                    id: 0,
                    error: { code: 0, message: `Could not list directory '${cliArguments[1]}'` },
                  }),
                )
              },
            )
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
              ...COOP_COEP_CORP_HEADERS,
            ])
            commandOutput.pipe(response, { end: true })
          }
          break
        }
        default: {
          response.writeHead(HTTP_STATUS_NOT_FOUND, COOP_COEP_CORP_HEADERS).end()
          break
        }
      }
    } else if (request.method === 'GET' && requestPath === '/api/root-directory') {
      response
        .writeHead(HTTP_STATUS_OK, [
          ['Content-Length', String(this.projectsRootDirectory.length)],
          ['Content-Type', 'text/plain'],
          ...COOP_COEP_CORP_HEADERS,
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
      for (const [header, value] of COOP_COEP_CORP_HEADERS) {
        response.setHeader(header, value)
      }
      fs.readFile(resourceFile)
        .then((data) => {
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

/** Details of a project. */
interface ProjectMetadata {
  /** The name of the project. */
  readonly name: string
  /** The namespace of the project. */
  readonly namespace: string
  /** The project id. */
  readonly id: string
  /**
   * The Enso Engine version to use for the project, represented by a semver version
   * string.
   *
   * If the edition associated with the project could not be resolved, the
   * engine version may be missing.
   */
  readonly engineVersion?: string
  /** The project creation time. */
  readonly created: string
  /** The last opened datetime. */
  readonly lastOpened?: string
}

/** Attributes of a file or folder. */
interface Attributes {
  readonly creationTime: string
  readonly lastAccessTime: string
  readonly lastModifiedTime: string
  readonly byteSize: number
}

/** Metadata for an arbitrary file system entry. */
type FileSystemEntry = DirectoryEntry | FileEntry | ProjectEntry

/** Metadata for a file. */
interface FileEntry {
  readonly type: 'FileEntry'
  readonly path: string
  readonly attributes: Attributes
}

/** Metadata for a directory. */
interface DirectoryEntry {
  readonly type: 'DirectoryEntry'
  readonly path: string
  readonly attributes: Attributes
}

/** Metadata for a project. */
interface ProjectEntry {
  readonly type: 'ProjectEntry'
  readonly path: string
  readonly metadata: ProjectMetadata
  readonly attributes: Attributes
}

/** A regex for matching hybrid project directories. */
export const HYBRID_PROJECT_DIRECTORY_MASK = /^cloud-project-\w+$/

/**
 * Checks if files that start with the dot.
 * Note on Windows does not check the hidden property.
 */
function isFileHidden(filePath: string): boolean {
  const dotfile = /(^|[\\/])\.[^\\/]+$/g
  return dotfile.test(filePath)
}

async function apiPmListDirectory(directoryPath: string, recursive = false) {
  const directoryPathQueue = [directoryPath]
  const entries: FileSystemEntry[] = []
  while (true) {
    const currentDirectoryPath = directoryPathQueue.shift()
    if (currentDirectoryPath == null) break
    const entryNames = await fs.readdir(currentDirectoryPath)
    for (const entryName of entryNames) {
      const entryPath = path.join(currentDirectoryPath, entryName)
      if (isFileHidden(entryPath)) continue
      const stat = await fs.stat(entryPath)
      const attributes: Attributes = {
        byteSize: stat.size,
        creationTime: new Date(stat.ctimeMs).toISOString(),
        lastAccessTime: new Date(stat.atimeMs).toISOString(),
        lastModifiedTime: new Date(stat.mtimeMs).toISOString(),
      }
      if (stat.isFile()) {
        entries.push({
          type: 'FileEntry',
          path: entryPath,
          attributes,
        } satisfies FileEntry)
      } else {
        if (recursive) {
          directoryPathQueue.push(entryPath)
        }
        try {
          const packageMetadataPath = path.join(entryPath, 'package.yaml')
          const projectMetadataPath = path.join(
            entryPath,
            projectManagement.PROJECT_METADATA_RELATIVE_PATH,
          )
          const packageMetadataContents = await fs.readFile(packageMetadataPath)
          const packageMetadataYaml = yaml.parse(packageMetadataContents.toString())
          let projectMetadataJson
          try {
            const projectMetadataContents = await fs.readFile(projectMetadataPath)
            projectMetadataJson = JSON.parse(projectMetadataContents.toString())
          } catch (e) {
            if ('name' in packageMetadataYaml && typeof packageMetadataYaml.name === 'string') {
              projectMetadataJson = {
                id: crypto.randomUUID(),
                kind: 'UserProject',
                created: new Date().toISOString(),
                lastOpened: null,
              }
              await fs.mkdir(path.dirname(projectMetadataPath), { recursive: true })
              await fs.writeFile(projectMetadataPath, JSON.stringify(projectMetadataJson))
            } else {
              throw e
            }
          }
          const metadata = extractProjectMetadata(packageMetadataYaml, projectMetadataJson)
          if (metadata != null) {
            // This is a project.
            entries.push({
              type: 'ProjectEntry',
              path: entryPath,
              attributes,
              metadata,
            } satisfies ProjectEntry)
          } else {
            // This error moves control flow to the
            // `catch` clause directly below.
            throw new Error('Invalid project metadata.')
          }
        } catch {
          // This is a regular directory, not a project.
          entries.push({
            type: 'DirectoryEntry',
            path: entryPath,
            attributes,
          } satisfies DirectoryEntry)
        }
      }
    }
  }
  return entries
}

/**
 * Return a {@link ProjectMetadata} if the metadata is a valid metadata object,
 * else return `null`.
 */
function extractProjectMetadata(yamlObj: unknown, jsonObj: unknown): ProjectMetadata | null {
  if (
    typeof yamlObj !== 'object' ||
    yamlObj == null ||
    typeof jsonObj !== 'object' ||
    jsonObj == null
  ) {
    return null
  } else {
    const validDateString = (string: string) => {
      const date = new Date(string)
      return !Number.isNaN(Number(date)) ? date.toString() : null
    }
    const name = 'name' in yamlObj && typeof yamlObj.name === 'string' ? yamlObj.name : null
    const namespace =
      'namespace' in yamlObj && typeof yamlObj.namespace === 'string' ? yamlObj.namespace : 'local'
    const engineVersion =
      'edition' in yamlObj && typeof yamlObj.edition === 'string' ? yamlObj.edition : null
    const id = 'id' in jsonObj && typeof jsonObj.id === 'string' ? jsonObj.id : null
    const created =
      'created' in jsonObj && typeof jsonObj.created === 'string' ?
        validDateString(jsonObj.created)
      : null
    const lastOpened =
      'lastOpened' in jsonObj && typeof jsonObj.lastOpened === 'string' ?
        validDateString(jsonObj.lastOpened)
      : null
    if (name != null && id != null && created != null) {
      return {
        name,
        namespace,
        id,
        ...(engineVersion != null ? { engineVersion } : {}),
        created,
        ...(lastOpened != null ? { lastOpened } : {}),
      } satisfies ProjectMetadata
    } else {
      return null
    }
  }
}
