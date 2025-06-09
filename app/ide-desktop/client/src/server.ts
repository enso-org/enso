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
import { COOP_COEP_CORP_HEADERS } from 'enso-common'
import * as ydocServer from 'ydoc-server'

import { tarFsPack, tarGzReadStreamToFs, unzipEntries, zipWriteStream } from '@/archive'
import * as contentConfig from '@/contentConfig'
import { BUNDLED_PROJECT_SUFFIX } from '@/fileAssociations'
import * as paths from '@/paths'
import { app } from 'electron'
import {
  AnyAsset,
  AssetConflict,
  AssetId,
  AssetType,
  DirectoryAsset,
  DirectoryId,
  ExportedArchive,
  extractTypeFromId,
  FileAsset,
  FileDetails,
  FileId,
  ImportArchiveResponse,
  ParentsPath,
  Path,
  ProjectAsset,
  ProjectId,
  ProjectState,
  S3FilePath,
  UnzipAssetsJobId,
  VirtualParentsPath,
} from 'enso-common/src/services/Backend'
import {
  DOWNLOAD_PROJECT_REGEX,
  downloadFilePath,
  EXPORT_ARCHIVE_PATH,
  GET_FILE_DETAILS_REGEX,
  IMPORT_ARCHIVE_PATH,
} from 'enso-common/src/services/Backend/remoteBackendPaths'
import { toRfc3339 } from 'enso-common/src/utilities/data/dateTime'
import { basenameAndExtension, getFileName, getFolderPath } from 'enso-common/src/utilities/file'
import { createReadStream, createWriteStream, statSync } from 'node:fs'
import {
  access,
  mkdir,
  mkdtemp,
  readdir,
  readFile,
  rename,
  rm,
  rmdir,
  stat,
  writeFile,
} from 'node:fs/promises'
import { tmpdir } from 'node:os'
import { finished } from 'node:stream/promises'
import { pathToFileURL } from 'node:url'
import { createGzip } from 'node:zlib'

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

// ==================
// === fileExists ===
// ==================

/** Return whether a file exists. */
async function fileExists(path: string) {
  try {
    await stat(path)
    return true
  } catch {
    return false
  }
}

// ========================
// === extractTypeAndId ===
// ========================

/** The internal asset type and properly typed corresponding internal ID of an arbitrary asset. */
interface AssetTypeAndIdRaw<Type extends AssetType> {
  readonly type: Type
  readonly path: Path
}

/** The internal asset type and properly typed corresponding internal ID of an arbitrary asset. */
type AssetTypeAndId<Id extends AssetId = AssetId> =
  | (DirectoryId extends Id ? AssetTypeAndIdRaw<AssetType.directory> : never)
  | (FileId extends Id ? AssetTypeAndIdRaw<AssetType.file> : never)
  | (ProjectId extends Id ? AssetTypeAndIdRaw<AssetType.project> : never)

export function extractTypeAndPath<Id extends AssetId>(id: Id): AssetTypeAndId<Id>
/**
 * Extracts the asset type and its corresponding internal ID from a {@link AssetId}.
 * @throws {Error} if the id has an unknown type.
 */
export function extractTypeAndPath<Id extends AssetId>(id: Id): AssetTypeAndId {
  const [, typeRaw, idRaw = ''] = id.match(/(.+?)-(.+)/) ?? []

  switch (typeRaw) {
    case AssetType.directory:
    case AssetType.project:
    case AssetType.file: {
      return {
        type: typeRaw,
        path: Path(decodeURIComponent(idRaw)),
      }
    }
    case undefined:
    default: {
      throw new Error(`Invalid type '${typeRaw}'`)
    }
  }
}

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
  private projectsRootDirectory: string
  private devServer?: vite.ViteDevServer

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
    } else if (requestUrl.startsWith('/api/cloud/')) {
      const route = new URL(`https://example.com${requestUrl.replace('/api/', '/')}`)
      const params = route.searchParams
      switch (route.pathname) {
        case '/cloud/download-project': {
          await this.httpCloudDownloadProject(request, response, params)
          break
        }
        case '/cloud/get-project-archive': {
          await this.httpCloudGetProjectArchive(request, response, params)
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
        case `/${EXPORT_ARCHIVE_PATH}`: {
          await this.httpDownloadArchive(request, response, params)
          break
        }
        case `/${IMPORT_ARCHIVE_PATH}`: {
          await this.httpUploadArchive(request, response, params)
          break
        }
        case '/upload-file': {
          await this.httpUploadFile(request, response, params)
          break
        }
        case '/upload-project': {
          // This endpoint should only be used when accessing the app from the browser.
          // When accessing the app from Electron, the file input event will have the
          // full system path.
          await this.httpUploadProject(request, response, params)
          break
        }
        case '/run-project-manager-command': {
          await this.httpRunProjectManagerCommand(request, response, params)
          break
        }
        default: {
          let match: RegExpMatchArray | null = null
          match = route.pathname.match(GET_FILE_DETAILS_REGEX)
          if (match?.groups?.['fileId'] != null) {
            const fileId = match.groups['fileId']
            await this.httpGetFileDetails(request, response, params, [fileId as FileId])
            break
          }
          match = route.pathname.match(DOWNLOAD_PROJECT_REGEX)
          if (match?.groups?.['projectId'] != null) {
            const projectId = match.groups['projectId']
            await this.httpDownloadProject(request, response, params, [projectId as ProjectId])
            break
          }
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
          break
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

  /** Send a HTTP response with a JSON payload. */
  httpOkJson(response: http.ServerResponse, body: unknown) {
    const content = JSON.stringify(body)
    return response
      .writeHead(HTTP_STATUS_OK, [
        ['Content-Length', `${content.length}`],
        ['Content-Type', 'application/json'],
        ...COOP_COEP_CORP_HEADERS,
      ])
      .end(content)
  }

  /** Response handler for "download project from cloud" endpoint. */
  async apiCloudDownloadProject(downloadUrl: string, projectId: ProjectId) {
    const response = await new Promise<http.IncomingMessage>((resolve) =>
      https.get(downloadUrl, resolve),
    )
    const projectsDirectory = projectManagement.getProjectsDirectory()
    const parentDirectory = path.join(projectsDirectory, `cloud-${projectId}`)
    const targetDirectory = path.join(parentDirectory, 'project_root')

    await mkdir(targetDirectory, { recursive: true })
    await projectManagement.unpackBundle(response, targetDirectory)
    return { targetDirectory, parentDirectory }
  }

  /** Response handler for "download project from cloud" endpoint. */
  async httpCloudDownloadProject(
    _request: http.IncomingMessage,
    response: http.ServerResponse,
    params: URLSearchParams,
  ) {
    const downloadUrl = this.expectParameter(response, params, 'downloadUrl')
    const projectId = this.expectParameter(response, params, 'projectId')
    if (downloadUrl == null || projectId == null) {
      return
    }

    try {
      this.httpOkJson(
        response,
        await this.apiCloudDownloadProject(downloadUrl, projectId as ProjectId),
      )
    } catch (error) {
      logger.error(error)
      const projectsDirectory = projectManagement.getProjectsDirectory()
      const parentDirectory = path.join(projectsDirectory, `cloud-${projectId}`)
      await access(parentDirectory)
        .then(() => {
          rmdir(parentDirectory, { maxRetries: 3, recursive: true })
        })
        .catch((e) => {
          logger.error(`Failed to cleanup directory ${parentDirectory}.`, e)
        })
      response.writeHead(HTTP_STATUS_INTERNAL_SERVER_ERROR, COOP_COEP_CORP_HEADERS).end()
    }
  }

  /** Return a parameter if it exists, return an error if it does not. */
  expectParameter(response: http.ServerResponse, params: URLSearchParams, parameter: string) {
    const value = params.get(parameter)
    if (value == null) {
      response
        .writeHead(HTTP_STATUS_BAD_REQUEST, COOP_COEP_CORP_HEADERS)
        .end(`Request is missing search parameter '${parameter}'.`)
    }
    return value
  }

  /** Response handler for "get project archive for cloud" endpoint. */
  async httpCloudGetProjectArchive(
    _request: http.IncomingMessage,
    response: http.ServerResponse,
    params: URLSearchParams,
  ) {
    const projectDir = this.expectParameter(response, params, 'directory')
    if (projectDir == null) {
      return
    }

    try {
      const projectBundle = await projectManagement.createBundle(projectDir)
      response
        .writeHead(HTTP_STATUS_OK, [
          ['Content-Length', String(projectBundle.byteLength)],
          ['Content-Type', 'application/octet-stream'],
          ...COOP_COEP_CORP_HEADERS,
        ])
        .end(projectBundle)
    } catch (error) {
      logger.error(error)
      response.writeHead(HTTP_STATUS_INTERNAL_SERVER_ERROR, COOP_COEP_CORP_HEADERS).end()
    }
  }

  /** Response handler for "get file details" endpoint. */
  async apiGetFileDetails(fileId: FileId) {
    const typeAndPath = extractTypeAndPath(fileId)
    const { path } = typeAndPath
    const file = this.apiGetAssetDetailsByPath(typeAndPath)
    if (file == null) {
      return
    }
    const stat = statSync(path)
    const result: FileDetails = {
      file: {
        fileId,
        fileName: getFileName(path),
        // Incorrect, but not sure what to do.
        path: S3FilePath(String(path)),
      },
      metadata: { size: stat.size },
      url: downloadFilePath(fileId),
    }
    return result
  }

  /** Response handler for "get file details" endpoint. */
  async httpGetFileDetails(
    _request: http.IncomingMessage,
    response: http.ServerResponse,
    _params: URLSearchParams,
    [fileId]: [fileId: FileId],
  ) {
    const details = await this.apiGetFileDetails(fileId)
    if (details) {
      return
    }
    this.httpOkJson(response, details)
  }

  /** Response handler for "download project" endpoint. */
  async httpDownloadProject(
    _request: http.IncomingMessage,
    response: http.ServerResponse,
    params: URLSearchParams,
    [projectId]: [projectId: ProjectId],
  ) {
    const filePath = params.get('filePath')
    const projectPath = extractTypeAndPath(projectId).path
    const stream = tarFsPack(projectPath).pipe(createGzip())
    let promise: Promise<void> | undefined
    if (filePath != null) {
      promise = finished(stream.pipe(createWriteStream(filePath)))
    } else {
      response.writeHead(HTTP_STATUS_OK, [
        ['Content-Type', 'application/octet-stream'],
        ...COOP_COEP_CORP_HEADERS,
      ])
      await finished(stream.pipe(response))
    }
    if (filePath == null) {
      return
    }
    await promise
    this.httpOkJson(response, null)
  }

  /** Create an archive stream with the given assets. */
  apiArchiveStream(assets: readonly AssetId[]) {
    const archive = zipWriteStream()

    const addProject = async (id: ProjectId, rootPath?: string) => {
      const assetPath = extractTypeAndPath(id).path
      rootPath ??= getFolderPath(assetPath)
      const pathInArchive = `${path.relative(rootPath, assetPath)}${BUNDLED_PROJECT_SUFFIX}`
      if (!(await fileExists(assetPath))) {
        return { type: 'error', error: 'notFound', id } as const
      }
      await archive.addFile(tarFsPack(assetPath).pipe(createGzip()), { name: pathInArchive })
    }

    const addFile = async (id: FileId, rootPath?: string) => {
      const assetPath = extractTypeAndPath(id).path
      rootPath ??= getFolderPath(assetPath)
      const pathInArchive = path.relative(rootPath, assetPath)
      if (!(await fileExists(assetPath))) {
        return { type: 'error', error: 'notFound', id } as const
      }
      await archive.addFile(createReadStream(assetPath), { name: pathInArchive })
    }

    const addFolder = async (id: DirectoryId, rootPath?: string) => {
      const assetPath = extractTypeAndPath(id).path
      rootPath ??= getFolderPath(assetPath)
      const pathInArchive = path.relative(rootPath, assetPath)
      if (!(await fileExists(assetPath))) {
        return { type: 'error', error: 'notFound', id } as const
      }
      await archive.addFolder({ name: pathInArchive })
      const entries = await this.apiListDirectory({ directory: id })
      for (const entry of entries) {
        await addAsset(entry.id, rootPath)
      }
    }

    const addAsset = async (id: AssetId, rootPath?: string) => {
      const typeAndId = extractTypeFromId(id)
      switch (typeAndId.type) {
        case AssetType.project: {
          const error = await addProject(typeAndId.id, rootPath)
          if (error) {
            return error
          }
          break
        }
        case AssetType.file: {
          const error = await addFile(typeAndId.id, rootPath)
          if (error) {
            return error
          }
          break
        }
        case AssetType.directory: {
          const error = await addFolder(typeAndId.id, rootPath)
          if (error) {
            return error
          }
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
          return
        }
      }
    }

    const promise = (async () => {
      for (const id of assets) {
        const error = await addAsset(id)
        if (error) {
          return error
        }
      }
      archive.finalize()
    })()

    return { stream: archive.stream, promise } as const
  }

  /** Response handler for "download archive" endpoint. */
  async httpDownloadArchive(
    _request: http.IncomingMessage,
    response: http.ServerResponse,
    params: URLSearchParams,
  ) {
    const assets = params.getAll('asset') as AssetId[]
    const filePath = params.get('filePath')
    const archive = this.apiArchiveStream(assets)
    let promise: Promise<void> | undefined
    if (filePath != null) {
      promise = finished(archive.stream.pipe(createWriteStream(filePath)))
    } else {
      response.writeHead(HTTP_STATUS_OK, [
        ['Content-Type', 'application/octet-stream'],
        ...COOP_COEP_CORP_HEADERS,
      ])
      await finished(archive.stream.pipe(response))
    }

    if (filePath == null) {
      // The HTTP headers were already sent
      return
    }
    const error = await archive.promise
    if (error) {
      const content = JSON.stringify({ error: `Asset '${error.id}' not found` })
      response
        .writeHead(HTTP_STATUS_NOT_FOUND, [
          ['Content-Length', String(content.length)],
          ['Content-Type', 'application/json'],
          ...COOP_COEP_CORP_HEADERS,
        ])
        .end(content)
    }
    await promise
    const result: ExportedArchive = { filePath: Path(filePath) }
    this.httpOkJson(response, result)
  }

  /** Get details for an asset by its path. */
  apiGetAssetDetailsByPath<Type extends AssetType>({
    type,
    path,
  }: {
    type?: Type
    path: Path
  }): AnyAsset<Type> | undefined {
    try {
      // @ts-expect-error This is UNSAFE if `Type` is specified explicitly.
      // If it is inferred, this means `type` is present and the constraint correctly falls back to
      // `AssetType`
      type ??= (() => {
        const assetStat = statSync(path)
        if (assetStat.isDirectory()) {
          const metadata = projectManagement.getMetadata(path)
          if (metadata) {
            return AssetType.project
          } else {
            return AssetType.directory
          }
        } else {
          return AssetType.file
        }
      })()
      const shared = {
        title: getFileName(path),
        modifiedAt: toRfc3339(new Date()),
        parentId: DirectoryId(`directory-${getFolderPath(path)}` as const),
        extension: null,
        permissions: [],
        projectState: null,
        parentsPath: ParentsPath(''),
        virtualParentsPath: VirtualParentsPath(''),
      } satisfies Partial<DirectoryAsset>
      switch (type) {
        case AssetType.project: {
          const result: ProjectAsset = {
            ...shared,
            type: AssetType.project,
            id: ProjectId(`project-${path}`),
            // FIXME: Get correct state.
            projectState: { type: ProjectState.closed },
          }
          // This is SAFE because `type` has been narrowed in the `switch` above.
          return result as AnyAsset<Type>
        }
        case AssetType.file: {
          const result: FileAsset = {
            ...shared,
            type: AssetType.file,
            id: FileId(`file-${path}`),
            extension: basenameAndExtension(path).extension,
          }
          // This is SAFE because `type` has been narrowed in the `switch` above.
          return result as AnyAsset<Type>
        }
        case AssetType.directory: {
          const result: DirectoryAsset = {
            ...shared,
            type: AssetType.directory,
            id: DirectoryId(`directory-${path}` as const),
          }
          // This is SAFE because `type` has been narrowed in the `switch` above.
          return result as AnyAsset<Type>
        }
        default: {
          throw new Error(`Unknown asset type '${type}'`)
        }
      }
    } catch {
      return
    }
  }

  /** Get an asset's details by its id. */
  apiGetAssetDetails({ assetId }: { readonly assetId: AssetId }) {
    const typeAndPath = extractTypeAndPath(assetId)
    return this.apiGetAssetDetailsByPath(typeAndPath)
  }

  /** List a directory. */
  async apiListDirectory(params: { readonly directory?: DirectoryId }) {
    const { directory: directoryRaw } = params
    const directory =
      directoryRaw ? extractTypeAndPath(directoryRaw).path : this.projectsRootDirectory
    const assets: AnyAsset[] = []
    for (const entryName of await readdir(directory)) {
      const entryPath = Path(path.join(directory, entryName))
      const asset = await this.apiGetAssetDetailsByPath({ path: entryPath })
      if (asset == null) {
        throw new Error(`File not found at '${entryPath}'`)
      }
      assets.push(asset)
    }
    return assets
  }

  /** Response handler for "upload archive" endpoint. */
  async httpUploadArchive(
    request: http.IncomingMessage,
    response: http.ServerResponse,
    params: URLSearchParams,
  ) {
    // eslint-disable-next-line @typescript-eslint/no-this-alias
    const self = this
    const directoryParam = params.get('directory') as DirectoryId | null
    const directory =
      directoryParam ? extractTypeAndPath(directoryParam).path : this.projectsRootDirectory
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
    const conflicts: AssetConflict[] = []
    for await (const { metadata } of await unzipEntries(filePath)) {
      const entryPathInArchive = metadata.name
      const entryPath = Path(path.join(directory, entryPathInArchive))
      const isDirectory = entryPathInArchive.endsWith('/')
      const isProject = entryPathInArchive.endsWith(BUNDLED_PROJECT_SUFFIX)
      // If directories need to be merged in the future, the 'existing asset' check can be skipped.
      const existingAsset = self.apiGetAssetDetailsByPath({ path: entryPath })
      if (existingAsset) {
        const conflict: AssetConflict = {
          type: existingAsset.type,
          path: Path(entryPathInArchive),
          existingAsset,
        }
        conflicts.push(conflict)
        continue
      }
      const shared = {
        title: getFileName(entryPath),
        modifiedAt: toRfc3339(new Date()),
        parentId: DirectoryId(`directory-${getFolderPath(entryPath)}` as const),
        extension: null,
        permissions: [],
        projectState: null,
        parentsPath: ParentsPath(''),
        virtualParentsPath: VirtualParentsPath(''),
      } satisfies Partial<DirectoryAsset>
      if (isDirectory) {
        assets.push({
          ...shared,
          type: AssetType.directory,
          id: DirectoryId(`directory-${entryPath}` as const),
        })
      } else if (isProject) {
        assets.push({
          ...shared,
          type: AssetType.project,
          id: ProjectId(`project-${entryPath.replace(BUNDLED_PROJECT_SUFFIX, '/')}`),
          projectState: { type: ProjectState.closed },
        })
      } else {
        assets.push({
          ...shared,
          type: AssetType.file,
          id: FileId(`file-${entryPath}`),
          extension: basenameAndExtension(entryPath).extension,
        })
      }
    }
    if (conflicts.length === 0) {
      // Upload; no conflict resolution needed.
      for await (const entry of await unzipEntries(filePath)) {
        if (entry.metadata.name.endsWith(BUNDLED_PROJECT_SUFFIX)) {
          const destinationPath = entry.getDestinationPath(directory)
          await entry.extract(directory, async (stream) => {
            await tarGzReadStreamToFs(stream, destinationPath)
            const entries = await readdir(destinationPath)
            const originalSingleChild = entries[0]
            // Unwrap project contents if there is only a single directory inside.
            if (entries.length === 1 && originalSingleChild != null) {
              let singleChild = originalSingleChild
              while (
                await fileExists(path.join(destinationPath, originalSingleChild, singleChild))
              ) {
                singleChild += '_'
              }
              if (singleChild !== originalSingleChild) {
                await rename(
                  path.join(destinationPath, originalSingleChild),
                  path.join(destinationPath, singleChild),
                )
              }
              const childPath = path.join(destinationPath, singleChild)
              for (const entry of await readdir(childPath)) {
                await rename(path.join(childPath, entry), path.join(destinationPath, entry))
              }
            }
            // Prevent default behavior.
            return false as const
          })
        } else {
          await entry.extract(directory)
        }
      }
      if (tempDirectory != null) {
        await rm(tempDirectory, { force: true, recursive: true })
      }
    }
    const result: ImportArchiveResponse =
      conflicts.length === 0 ? { assets } : { jobId: UnzipAssetsJobId(filePath), conflicts }
    this.httpOkJson(response, result)
  }

  /** Response handler for "upload file" endpoint. */
  async httpUploadFile(
    request: http.IncomingMessage,
    response: http.ServerResponse,
    params: URLSearchParams,
  ) {
    const fileName = params.get('file_name')
    const directoryParam = params.get('directory') as DirectoryId | null
    const directory =
      directoryParam ? extractTypeAndPath(directoryParam).path : this.projectsRootDirectory
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

  /** Response handler for "upload project" endpoint. */
  async httpUploadProject(
    request: http.IncomingMessage,
    response: http.ServerResponse,
    params: URLSearchParams,
  ) {
    const directoryParam = params.get('directory') as DirectoryId | null
    const directory = directoryParam ? extractTypeAndPath(directoryParam).path : null
    const name = params.get('name')
    try {
      const project = await this.config.externalFunctions.uploadProjectBundle(
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

  /** Response handler for "run project manager command" endpoint. */
  async httpRunProjectManagerCommand(
    request: http.IncomingMessage,
    response: http.ServerResponse,
    params: URLSearchParams,
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
  }
}
