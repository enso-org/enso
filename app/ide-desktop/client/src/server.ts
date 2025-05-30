/** @file A simple HTTP server which serves application data to the Electron web-view. */

import * as mkcert from 'mkcert'
import * as http from 'node:http'
import * as https from 'node:https'
import * as path from 'node:path'
import * as stream from 'node:stream'

import createServer from 'create-servers'
import * as mime from 'mime-types'
import * as portfinder from 'portfinder'
import type * as vite from 'vite'

import * as projectManagement from '@/projectManagement'
import { COOP_COEP_CORP_HEADERS, PRODUCT_NAME } from 'enso-common'
import GLOBAL_CONFIG from 'enso-common/src/config.json' with { type: 'json' }
import * as ydocServer from 'ydoc-server'

import * as contentConfig from '@/contentConfig'
import * as paths from '@/paths'
import { app } from 'electron'
import {
  AnyAsset,
  AssetId,
  AssetType,
  DirectoryAsset,
  DirectoryId,
  ExportedArchive,
  extractTypeFromId,
  FileId,
  ParentsPath,
  Path,
  ProjectId,
  ProjectState,
  VirtualParentsPath,
} from 'enso-common/src/services/Backend'
import { toReadableIsoString, toRfc3339 } from 'enso-common/src/utilities/data/dateTime'
import { basenameAndExtension, getFileName, getFolderPath } from 'enso-common/src/utilities/file'
import { createWriteStream } from 'node:fs'
import {
  access,
  mkdir,
  mkdtemp,
  readdir,
  readFile,
  rm,
  rmdir,
  stat,
  writeFile,
} from 'node:fs/promises'
import { tmpdir } from 'node:os'
import { finished } from 'node:stream/promises'
import { pathToFileURL } from 'node:url'
import { Unzip, Zip } from 'zip-lib'

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

/** Return whether a file exists. */
async function exists(path: string) {
  try {
    await stat(path)
    return true
  } catch {
    return false
  }
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
  async process(request: http.IncomingMessage, response: http.ServerResponse) {
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
      const route = new URL(`https://example.com${requestUrl.replace('/api/', '/')}`)
      const params = route.searchParams
      switch (route.pathname) {
        case '/cloud/download-project': {
          await apiDownloadProject(request, response, params)
          break
        }
        case '/cloud/get-project-archive': {
          await apiGetProjectArchive(request, response, params)
          break
        }
        default: {
          logger.error(`Unknown Cloud middleware request:`, requestPath)
          break
        }
      }
    } else if (request.method === 'POST' && request.url?.startsWith('/api/')) {
      const route = new URL(`https://example.com${requestUrl.replace('/api/', '/')}`)
      const params = route.searchParams
      switch (route.pathname) {
        case '/files/download-archive': {
          await apiDownloadArchive(request, response, params)
          break
        }
        case '/files/upload-archive': {
          await apiUploadArchive(request, response, params, this)
          break
        }
        case '/upload-file': {
          await apiUploadFile(request, response, params, this)
          break
        }
        case '/upload-project': {
          // This endpoint should only be used when accessing the app from the browser.
          // When accessing the app from Electron, the file input event will have the
          // full system path.
          await apiUploadProject(request, response, params, this)
          break
        }
        case '/run-project-manager-command': {
          await apiRunProjectManagerCommand(request, response, params, this)
          break
        }
        default: {
          const content = JSON.stringify({
            type: 'error',
            error: `Unknown endpoint '${route.pathname}'`,
          })
          response
            .writeHead(HTTP_STATUS_NOT_FOUND, [
              ['Content-Length', String(content.length)],
              ['Content-Type', 'application/json'],
              ...COOP_COEP_CORP_HEADERS,
            ])
            .end(content)
          return
        }
      }
    } else if (request.method === 'GET' && requestPath?.startsWith('/api/')) {
      const route = new URL(`https://example.com${requestUrl.replace('/api/', '/')}`)
      switch (route.pathname) {
        case '/root-directory': {
          const path = this.projectsRootDirectory
          response
            .writeHead(HTTP_STATUS_OK, [
              ['Content-Length', String(path.length)],
              ['Content-Type', 'text/plain'],
              ...COOP_COEP_CORP_HEADERS,
            ])
            .end(path)
          break
        }
        case '/download-directory': {
          const path = app.getPath('downloads')
          response
            .writeHead(HTTP_STATUS_OK, [
              ['Content-Length', String(path.length)],
              ['Content-Type', 'text/plain'],
              ...COOP_COEP_CORP_HEADERS,
            ])
            .end(path)
          break
        }
      }
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
      readFile(resourceFile)
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

async function apiDownloadProject(
  _request: http.IncomingMessage,
  response: http.ServerResponse,
  params: URLSearchParams,
) {
  const downloadUrl = params.get('downloadUrl')
  const projectId = params.get('projectId')

  if (downloadUrl == null) {
    response
      .writeHead(HTTP_STATUS_BAD_REQUEST, COOP_COEP_CORP_HEADERS)
      .end('Request is missing search parameter `downloadUrl`.')
    return
  }

  if (projectId == null) {
    response
      .writeHead(HTTP_STATUS_BAD_REQUEST, COOP_COEP_CORP_HEADERS)
      .end('Request is missing search parameter `projectId`.')
    return
  }

  https.get(downloadUrl, async (actualResponse) => {
    const projectsDirectory = projectManagement.getProjectsDirectory()
    const parentDirectory = path.join(projectsDirectory, `cloud-${projectId}`)
    const targetDirectory = path.join(parentDirectory, 'project_root')

    try {
      await mkdir(targetDirectory, { recursive: true })
      await projectManagement.unpackBundle(actualResponse, targetDirectory)
      response
        .writeHead(HTTP_STATUS_OK, COOP_COEP_CORP_HEADERS)
        .end(JSON.stringify({ targetDirectory, parentDirectory }))
    } catch (e) {
      logger.error(e)
      await access(parentDirectory)
        .then(() => {
          rmdir(parentDirectory, { maxRetries: 3, recursive: true })
        })
        .catch((e) => {
          logger.error(`Failed to cleanup directory ${parentDirectory}.`, e)
        })
      response.writeHead(HTTP_STATUS_INTERNAL_SERVER_ERROR, COOP_COEP_CORP_HEADERS).end()
    }
  })
}

async function apiGetProjectArchive(
  _request: http.IncomingMessage,
  response: http.ServerResponse,
  params: URLSearchParams,
) {
  const projectDir = params.get('directory')

  if (projectDir == null) {
    response
      .writeHead(HTTP_STATUS_BAD_REQUEST, COOP_COEP_CORP_HEADERS)
      .end('Request is missing search parameter `directory`.')
    return
  }

  try {
    const projectBundle = await projectManagement.createBundle(projectDir)
    response
      .writeHead(HTTP_STATUS_OK, {
        ...COOP_COEP_CORP_HEADERS,
        'Content-Length': String(projectBundle.byteLength),
      })
      .end(projectBundle)
  } catch (error) {
    logger.error(error)
    response.writeHead(HTTP_STATUS_INTERNAL_SERVER_ERROR, COOP_COEP_CORP_HEADERS).end()
  }
}

async function apiDownloadArchive(
  _request: http.IncomingMessage,
  response: http.ServerResponse,
  params: URLSearchParams,
) {
  const assets = params.getAll('asset') as AssetId[]
  const archive = new Zip()
  let filePath = params.get('filePath')
  const notFound = (id: AssetId) => {
    const content = JSON.stringify({ error: `Asset '${id}' not found` })
    response
      .writeHead(HTTP_STATUS_NOT_FOUND, [
        ['Content-Length', String(content.length)],
        ['Content-Type', 'application/json'],
        ...COOP_COEP_CORP_HEADERS,
      ])
      .end(content)
  }
  for (const asset of assets) {
    const typeAndId = extractTypeFromId(asset)
    switch (typeAndId.type) {
      case AssetType.project: {
        const [, uuid = '', directory = ''] =
          asset.replace(/^project-/, '').match(/(\w+-\w+-\w+-\w+-\w+)-(.+)/) ?? []
        const entries = await readdir(directory, { withFileTypes: true })
        let found = false
        for (const entry of entries) {
          if (entry.isFile()) {
            continue
          }
          try {
            const projectPath = path.join(entry.parentPath, entry.name)
            const metadata = projectManagement.getMetadata(projectPath)
            if (metadata?.id !== uuid) {
              continue
            }
            archive.addFolder(projectPath, entry.name)
            found = true
            break
          } catch {
            // Ignore; this folder is not a project entry.
          }
        }
        if (!found) {
          notFound(asset)
          return
        }
        break
      }
      case AssetType.file: {
        const filePath = asset.replace(/^file-/, '')
        if (!(await exists(filePath))) {
          notFound(asset)
          return
        }
        archive.addFile(filePath, getFileName(filePath))
        break
      }
      case AssetType.directory: {
        const directoryPath = asset.replace(/^directory-/, '')
        if (!(await exists(directoryPath))) {
          notFound(asset)
          return
        }
        archive.addFolder(directoryPath, getFileName(directoryPath))
        break
      }
      // These asset types are not valid, however include them to force any newly added
      // asset types to be handled (by causing a non-exhaustiveness error).
      case AssetType.secret:
      case AssetType.datalink:
      case AssetType.specialLoading:
      case AssetType.specialEmpty:
      case AssetType.specialError:
      case AssetType.specialUp: {
        continue
      }
    }
  }
  if (filePath == null) {
    const folderPath = app.getPath('downloads')
    let generatedFilePath: string
    let number = 0
    do {
      number += 1
      const secondsString = new Date().getSeconds().toString().padStart(2, '0')
      const dateString = `${toReadableIsoString(new Date()).replace(/[:]/g, ' ')} ${secondsString}`
      const suffix = number === 1 ? '' : ` (${number})`
      generatedFilePath = path.join(
        folderPath,
        `${PRODUCT_NAME} archive ${dateString}${suffix}.zip`,
      )
    } while (await exists(generatedFilePath))
    filePath = generatedFilePath
  }
  await archive.archive(filePath)
  const result: ExportedArchive = { filePath: Path(filePath) }
  const content = JSON.stringify(result)
  response
    .writeHead(HTTP_STATUS_OK, [
      ['Content-Length', String(content.length)],
      ['Content-Type', 'application/json'],
      ...COOP_COEP_CORP_HEADERS,
    ])
    .end(content)
}

async function apiUploadArchive(
  request: http.IncomingMessage,
  response: http.ServerResponse,
  params: URLSearchParams,
  self: Server,
) {
  const directory =
    params.get('directory')?.replace(/^directory-/, '') ?? self.projectsRootDirectory
  let filePath = params.get('filePath')
  let tempDirectory: string | undefined
  if (filePath == null) {
    tempDirectory = await mkdtemp(path.join(tmpdir(), 'enso-'))
    filePath = path.join(tempDirectory, 'archive.zip')
    const writeStream = createWriteStream(filePath)
    request.pipe(writeStream)
    await finished(writeStream)
  }
  const assets: AnyAsset[] = []
  const archive = new Unzip({
    onEntry(event) {
      const childPath = path.join(directory, event.entryName)
      const shared = {
        title: getFileName(childPath),
        modifiedAt: toRfc3339(new Date()),
        parentId: DirectoryId(`directory-${getFolderPath(childPath)}` as const),
        extension: null,
        permissions: [],
        projectState: null,
        parentsPath: ParentsPath(''),
        virtualParentsPath: VirtualParentsPath(''),
      } satisfies Partial<DirectoryAsset>
      if (event.entryName.endsWith('/')) {
        assets.push({
          ...shared,
          type: AssetType.directory,
          id: DirectoryId(`directory-${childPath}` as const),
        })
      } else {
        assets.push({
          ...shared,
          type: AssetType.file,
          id: FileId(`file-${childPath}`),
          extension: basenameAndExtension(childPath).extension,
        })
      }
    },
  })
  await archive.extract(filePath, directory)
  if (tempDirectory != null) {
    await rm(tempDirectory, { force: true, recursive: true })
  }
  for (let i = 0; i < assets.length; i += 1) {
    const asset = assets[i]
    if (asset?.type !== AssetType.directory) {
      continue
    }
    const path = asset.id.replace('directory-', '')
    const metadata = projectManagement.getMetadata(path)
    if (!metadata) {
      // Ignore; this folder is not a project.
      continue
    }
    assets[i] = {
      ...asset,
      type: AssetType.project,
      id: ProjectId(`project-${metadata.id}-${asset.parentId.replace('directory-', '')}`),
      projectState: { type: ProjectState.closed },
    }
  }
  const content = JSON.stringify(assets)
  response
    .writeHead(HTTP_STATUS_OK, [
      ['Content-Length', String(content.length)],
      ['Content-Type', 'application/json'],
      ...COOP_COEP_CORP_HEADERS,
    ])
    .end(content)
}

async function apiUploadFile(
  request: http.IncomingMessage,
  response: http.ServerResponse,
  params: URLSearchParams,
  self: Server,
) {
  const fileName = params.get('file_name')
  const directory =
    params.get('directory')?.replace(/^directory-/, '') ?? self.projectsRootDirectory
  if (fileName == null) {
    response
      .writeHead(HTTP_STATUS_BAD_REQUEST, COOP_COEP_CORP_HEADERS)
      .end('Request is missing search parameter `file_name`.')
  } else {
    const filePath = path.join(directory, fileName)
    void writeFile(filePath, request)
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
}

async function apiUploadProject(
  request: http.IncomingMessage,
  response: http.ServerResponse,
  params: URLSearchParams,
  self: Server,
) {
  const directory = params.get('directory')?.replace(/^directory-/, '') ?? null
  const name = params.get('name')
  try {
    const project = await self.config.externalFunctions.uploadProjectBundle(
      request,
      directory,
      name,
    )
    response
      .writeHead(HTTP_STATUS_OK, [
        ['Content-Length', String(project.id.length)],
        ['Content-Type', 'text/plain'],
        ...COOP_COEP_CORP_HEADERS,
      ])
      .end(project.id)
  } catch {
    response.writeHead(HTTP_STATUS_BAD_REQUEST, COOP_COEP_CORP_HEADERS).end()
  }
}

async function apiRunProjectManagerCommand(
  request: http.IncomingMessage,
  response: http.ServerResponse,
  params: URLSearchParams,
  self: Server,
) {
  const cliArguments: unknown = JSON.parse(params.get('cli-arguments') ?? '[]')
  if (
    !Array.isArray(cliArguments) ||
    !cliArguments.every((item): item is string => typeof item === 'string')
  ) {
    response
      .writeHead(HTTP_STATUS_BAD_REQUEST, COOP_COEP_CORP_HEADERS)
      .end('Command arguments must be an array of strings.')
  } else {
    const commandOutput = (() => {
      try {
        return self.config.externalFunctions.runProjectManagerCommand(cliArguments, request)
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
}
