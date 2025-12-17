/** @file A simple HTTP server which serves application data to the Electron web-view. */

import * as mkcert from 'mkcert'
import * as http from 'node:http'
import * as path from 'node:path'
import * as stream from 'node:stream'
import * as streamConsumers from 'node:stream/consumers'

import createServer from 'create-servers'
import * as mime from 'mime-types'
import * as portfinder from 'portfinder'
import type * as vite from 'vite'

import { COOP_COEP_CORP_HEADERS } from 'enso-common/src/constants'
import * as projectManagement from 'project-manager-shim'
import type { Watcher } from 'project-manager-shim/fs'
import {
  handleFilesystemCommand,
  handleProjectServiceRequest,
  handleWatcherRequest,
  isProjectServiceRequest,
  isWatcherRequest,
} from 'project-manager-shim/handler'
import * as ydocServer from 'ydoc-server'

import { tarFsPack, unzipEntries, zipWriteStream } from '@/archive'
import { downloadCloudProject } from '@/assetManagement'
import { BUNDLED_PROJECT_SUFFIX } from '@/fileAssociations'
import * as paths from '@/paths'
import * as electron from 'electron'
import { app } from 'electron'
import {
  type AnyAsset,
  AssetId,
  AssetType,
  type DirectoryAsset,
  DirectoryId,
  EnsoPath,
  type ExportedArchive,
  extractTypeFromId,
  type FileAsset,
  type FileDetails,
  FileId,
  fileNameIsArchive,
  fileNameIsProject,
  ParentsPath,
  Path,
  type ProjectAsset,
  ProjectId,
  ProjectState,
  S3FilePath,
  stripProjectExtension,
  UnzipAssetsJobId,
  VirtualParentsPath,
} from 'enso-common/src/services/Backend'
import {
  DOWNLOAD_PROJECT_REGEX,
  downloadFilePath,
  EXPORT_ARCHIVE_PATH,
  GET_FILE_DETAILS_REGEX,
} from 'enso-common/src/services/Backend/remoteBackendPaths'
import { createReadStream, createWriteStream, statSync } from 'node:fs'
import { access, mkdtemp, readdir, readFile, rm, stat, writeFile } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import { finished } from 'node:stream/promises'
import { pathToFileURL } from 'node:url'
import { createGzip } from 'node:zlib'
import { ProjectService } from 'project-manager-shim/projectService'

ydocServer.configureAllDebugLogs(
  process.env.ENSO_IDE_YDOC_LS_DEBUG === 'true',
  // eslint-disable-next-line no-restricted-properties
  console.debug.bind(console),
)

// =================
// === Constants ===
// =================

const HTTP_STATUS_OK = 200
const HTTP_STATUS_BAD_REQUEST = 400
const HTTP_STATUS_NOT_FOUND = 404
const HTTP_STATUS_INTERNAL_SERVER_ERROR = 500

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

/** Constructor parameter for the server configuration. */
interface ConfigConfig {
  readonly dir: string
  readonly port: number
}

/** Server configuration. */
export class Config {
  dir: string
  port: number

  /** Create a server configuration. */
  constructor(cfg: ConfigConfig) {
    this.dir = path.resolve(cfg.dir)
    this.port = cfg.port
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

/**
 * A simple server implementation.
 *
 * Initially it was based on `union`, but later we migrated to `create-servers`.
 * Read this topic to learn why: https://github.com/http-party/http-server/issues/483
 */
export class Server {
  private projectsRootDirectory: string
  private devServer?: vite.ViteDevServer
  private projectService: ProjectService
  private watchers: Map<AssetId, Watcher> = new Map()

  /** Create a simple HTTP server. */
  constructor(
    public config: Config,
    projectService: ProjectService,
  ) {
    this.projectsRootDirectory = projectManagement.getProjectsDirectory()
    this.projectService = projectService
  }

  /** Server constructor. */
  static async create(config: Config, projectService: ProjectService): Promise<Server> {
    const localConfig = Object.assign({}, config)
    localConfig.port = await findPort(localConfig.port)
    const server = new Server(localConfig, projectService)
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
        async (err, { https: httpsServer, http: httpServer }) => {
          const server = httpsServer ?? httpServer
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
              mode: process.env.MODE ?? 'staging',
            })
          }
          if (err) {
            console.error('Error creating server:', err.http)
            reject(err)
          }
          resolve()
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
    if (requestUrl == null) {
      console.error('Request URL is null.')
    } else if (isProjectServiceRequest(requestUrl)) {
      const headers = Object.fromEntries(COOP_COEP_CORP_HEADERS)
      handleProjectServiceRequest(
        request,
        response,
        requestUrl,
        async () => this.projectService,
        headers,
      )
    } else if (isWatcherRequest(requestUrl)) {
      const headers = Object.fromEntries(COOP_COEP_CORP_HEADERS)
      handleWatcherRequest(request, response, headers, this.watchers)
    } else if (request.url?.startsWith('/api/')) {
      const route = new URL(`https://example.com${requestUrl.replace('/api/', '/')}`)
      const params = route.searchParams
      switch (`${request.method} ${route.pathname}`) {
        case 'GET /cloud/download-project': {
          await this.httpCloudDownloadProject(request, response, params)
          break
        }
        case 'GET /cloud/get-project-archive': {
          await this.httpCloudGetProjectArchive(request, response, params)
          break
        }
        case 'GET /root-directory-path': {
          await this.httpGetRootDirectoryPath(request, response, params)
          break
        }
        case 'GET /download-directory-path': {
          await this.httpGetDownloadDirectoryPath(request, response, params)
          break
        }
        // `GET` needs to be supported to be able to download the file using a hyperlink.
        case `GET /${EXPORT_ARCHIVE_PATH}`:
        case `POST /${EXPORT_ARCHIVE_PATH}`: {
          await this.httpDownloadArchive(request, response, params)
          break
        }
        case 'POST /upload-file': {
          await this.httpUploadFile(request, response, params)
          break
        }
        case 'POST /run-project-manager-command': {
          await this.httpRunProjectManagerCommand(request, response, params)
          break
        }
        default: {
          let match: RegExpMatchArray | null = null
          match = route.pathname.match(GET_FILE_DETAILS_REGEX)
          if (request.method === 'GET' && match?.groups?.['fileId'] != null) {
            const fileId = match.groups['fileId']
            await this.httpGetFileDetails(request, response, params, [fileId as FileId])
            break
          }
          match = route.pathname.match(DOWNLOAD_PROJECT_REGEX)
          if (request.method === 'GET' && match?.groups?.['projectId'] != null) {
            const projectId = match.groups['projectId']
            await this.httpDownloadProject(request, response, params, [projectId as ProjectId])
            break
          }
          console.error(`Unknown Cloud middleware request:`, route.pathname)
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
    } else if (this.devServer) {
      this.devServer.middlewares(request, response)
    } else {
      const url = requestUrl.split('?')[0] ?? ''

      // if it's a path of directory or project, it should be handled by application (index.html)
      // otherwise it's a path to some asset.
      const extension = path.extname(url)
      const resource = extension !== '' && extension !== '.project' ? requestUrl : '/index.html'

      // `preload.mjs` must be specialcased here as it is loaded by electron from the root,
      // in contrast to all assets loaded by the window, which are loaded from `assets/` via
      // this server.
      const resourceFile =
        resource === '/preload.mjs.map' ?
          paths.appPath(electron) + resource
        : this.config.dir + resource
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
          console.error(`Resource '${resource}' not found at '${resourceFile}'.`)
          response.writeHead(HTTP_STATUS_NOT_FOUND)
          response.end()
        })
    }
  }

  /** Send a HTTP response with a JSON payload. */
  httpOkJson<T = never>(response: http.ServerResponse, body: NoInfer<T>) {
    const content = JSON.stringify(body)
    return response
      .writeHead(HTTP_STATUS_OK, [
        ['Content-Length', `${content.length}`],
        ['Content-Type', 'application/json'],
        ...COOP_COEP_CORP_HEADERS,
      ])
      .end(content)
  }

  /** Send a HTTP response with a JSON payload. */
  httpOkText(response: http.ServerResponse, content: string) {
    return response
      .writeHead(HTTP_STATUS_OK, [
        ['Content-Length', `${content.length}`],
        ['Content-Type', 'text/plain'],
        ...COOP_COEP_CORP_HEADERS,
      ])
      .end(content)
  }

  /** Send a HTTP error with a text payload. */
  httpError(response: http.ServerResponse, message: string) {
    return response
      .writeHead(HTTP_STATUS_BAD_REQUEST, [
        ['Content-Length', `${message.length}`],
        ['Content-Type', 'text/plain'],
        ...COOP_COEP_CORP_HEADERS,
      ])
      .end(message)
  }

  /** The root directory path. */
  apiGetRootDirectoryPath() {
    return this.projectsRootDirectory
  }

  /** Response handler for "get root directory path" endpoint. */
  async httpGetRootDirectoryPath(
    _request: http.IncomingMessage,
    response: http.ServerResponse,
    _params: URLSearchParams,
  ) {
    this.httpOkText(response, this.apiGetRootDirectoryPath())
  }

  /** The download directory path. */
  apiGetDownloadDirectoryPath() {
    return app.getPath('downloads')
  }

  /** Response handler for "get download directory path" endpoint. */
  async httpGetDownloadDirectoryPath(
    _request: http.IncomingMessage,
    response: http.ServerResponse,
    _params: URLSearchParams,
  ) {
    this.httpOkText(response, this.apiGetDownloadDirectoryPath())
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
      this.httpOkJson<{
        readonly projectRootDirectory: string
        readonly parentDirectory: string
      }>(response, await downloadCloudProject(downloadUrl, ProjectId(projectId)))
    } catch (error) {
      console.error(error)
      const projectsDirectory = projectManagement.getProjectsDirectory()
      const parentDirectory = path.join(projectsDirectory, `cloud-${projectId}`)
      await access(parentDirectory)
        .then(() => {
          rm(parentDirectory, { maxRetries: 3, recursive: true, force: true })
        })
        .catch((e) => {
          console.error(`Failed to cleanup directory ${parentDirectory}.`, e)
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
    const parentDir = this.expectParameter(response, params, 'directory')
    if (parentDir == null) {
      response
        .writeHead(HTTP_STATUS_BAD_REQUEST, COOP_COEP_CORP_HEADERS)
        .end('Request is missing search parameter `directory`.')
      return
    }
    const projectDir = path.join(parentDir, 'project_root')

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
      console.error(error)
      response.writeHead(HTTP_STATUS_INTERNAL_SERVER_ERROR, COOP_COEP_CORP_HEADERS).end()
    }
  }

  /** Response handler for "get file details" endpoint. */
  async apiGetFileDetails(fileId: FileId) {
    const typeAndPath = extractTypeAndPath(fileId)
    const { path: filePath } = typeAndPath
    const file = this.apiGetAssetDetailsByPath(typeAndPath)
    if (file == null) {
      return
    }
    const stat = statSync(filePath)
    const result: FileDetails = {
      file: {
        fileId,
        fileName: path.basename(filePath),
        // Incorrect, but not sure what to do.
        path: S3FilePath(String(filePath)),
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
    if (!details) {
      const filePath = extractTypeAndPath(fileId).path
      this.httpError(response, `File not found at '${filePath}'`)
      return
    }
    this.httpOkJson<FileDetails>(response, details)
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
    this.httpOkJson<null>(response, null)
  }

  /** Upload an archive, optionally with a list of conflict resolutions. */
  async apiUploadArchive({
    directoryId,
    jobId,
    filePath,
    readStream,
  }: {
    directoryId?: DirectoryId | null | undefined
    jobId?: UnzipAssetsJobId | null | undefined
    filePath?: string | null | undefined
    readStream?: stream.Readable | null | undefined
  }): Promise<unknown> {
    filePath ??= jobId != null ? Path(decodeURIComponent(jobId)) : undefined
    const directory =
      directoryId ? extractTypeAndPath(directoryId).path : this.projectsRootDirectory
    let tempDirectory: string | undefined
    if (filePath == null) {
      tempDirectory = await mkdtemp(path.join(tmpdir(), 'enso-'))
      filePath = path.join(tempDirectory, 'archive.zip')
      const writeStream = createWriteStream(filePath)
      if (readStream == null) {
        throw new Error(
          'If no source path is provided, then a stream (e.g. from a request) is required',
        )
      }
      readStream.pipe(writeStream)
      await finished(writeStream)
    }
    const assets: AnyAsset[] = []

    const pathMapping: Record<string, string> = {}

    const getDirectoryPath = async (entryPathInArchive: string) => {
      const isDirectory = /[/\\]$/.test(entryPathInArchive)
      const parentPathInArchiveRaw = path.dirname(entryPathInArchive)
      let parentPathInArchive =
        parentPathInArchiveRaw === entryPathInArchive ? '' : pathMapping[parentPathInArchiveRaw]
      if (parentPathInArchive == null) {
        await getDirectoryPath(parentPathInArchiveRaw)
        parentPathInArchive = pathMapping[parentPathInArchiveRaw] ?? ''
      }
      const extensionRaw = path.extname(entryPathInArchive)
      const basenameRaw = path.basename(entryPathInArchive, extensionRaw)
      const basename = basenameRaw.match(/^.*(?= \((?:copy)? ?\d*\)$)/)?.[0] ?? basenameRaw
      const extension = (() => {
        switch (extensionRaw) {
          case 'enso-project':
          case 'tar.gz':
          case '': {
            return ''
          }
          default: {
            return `.${extensionRaw}`
          }
        }
      })()
      let destinationPathInArchive = path.join(parentPathInArchive, `${basename}${extension}`)
      let destinationPath = Path(path.join(directory, destinationPathInArchive))
      // If directories need to be merged in the future, the following check can be skipped
      // for directories.
      let i = 0
      while (await fileExists(destinationPath)) {
        i += 1
        destinationPathInArchive = path.join(parentPathInArchive, `${basename} (${i})${extension}`)
        destinationPath = Path(path.join(directory, destinationPathInArchive))
      }
      if (isDirectory) {
        pathMapping[entryPathInArchive] = destinationPathInArchive
      }
      return destinationPath
    }

    for await (const entry of await unzipEntries(filePath)) {
      const entryPathInArchive = entry.metadata.name
      const destinationPath = await getDirectoryPath(entryPathInArchive)
      const isDirectory = /[/\\]$/.test(entryPathInArchive)
      const isProject = entryPathInArchive.endsWith(BUNDLED_PROJECT_SUFFIX)
      const shared = {
        title: path.basename(destinationPath),
        modifiedAt: new Date().toISOString() as DirectoryAsset['modifiedAt'],
        parentId: DirectoryId(`directory-${path.dirname(destinationPath)}` as const),
        extension: null,
        permissions: [],
        projectState: null,
        parentsPath: ParentsPath(''),
        virtualParentsPath: VirtualParentsPath(''),
        ensoPath: EnsoPath(String(destinationPath)),
      } satisfies Partial<DirectoryAsset>
      if (isDirectory) {
        assets.push({
          ...shared,
          type: AssetType.directory,
          id: DirectoryId(`directory-${destinationPath}` as const),
        })
        await entry.extract({ rootDirectory: directory, destinationPath })
      } else if (isProject) {
        assets.push({
          ...shared,
          type: AssetType.project,
          id: ProjectId(`project-${destinationPath.replace(BUNDLED_PROJECT_SUFFIX, '/')}`),
          projectState: { type: ProjectState.closed },
        })
        await entry.extract({
          rootDirectory: directory,
          transform: async (stream) => {
            const parentDirectory = path.dirname(destinationPath)
            const fileName = path.basename(destinationPath)
            await projectManagement.uploadBundle(
              stream,
              parentDirectory,
              stripProjectExtension(fileName),
            )
            // Prevent default behavior.
            return false as const
          },
        })
      } else {
        assets.push({
          ...shared,
          type: AssetType.file,
          id: FileId(`file-${destinationPath}`),
          extension: path.extname(destinationPath),
        })
        await entry.extract({ rootDirectory: directory, destinationPath })
      }
    }
    if (tempDirectory != null) {
      await rm(tempDirectory, { force: true, recursive: true })
    }
    return { assets }
  }

  /** Create an archive stream with the given assets. */
  apiArchiveStream(assets: readonly AssetId[]) {
    const archive = zipWriteStream()

    const addProject = async (id: ProjectId, rootPath?: string) => {
      const assetPath = extractTypeAndPath(id).path
      rootPath ??= path.dirname(assetPath)
      const pathInArchive = `${path.relative(rootPath, assetPath)}${BUNDLED_PROJECT_SUFFIX}`
      if (!(await fileExists(assetPath))) {
        return { type: 'error', error: 'notFound', id } as const
      }
      await archive.addFile(tarFsPack(assetPath).pipe(createGzip()), { name: pathInArchive })
    }

    const addFile = async (id: FileId, rootPath?: string) => {
      const assetPath = extractTypeAndPath(id).path
      rootPath ??= path.dirname(assetPath)
      const pathInArchive = path.relative(rootPath, assetPath)
      if (!(await fileExists(assetPath))) {
        return { type: 'error', error: 'notFound', id } as const
      }
      await archive.addFile(createReadStream(assetPath), { name: pathInArchive })
    }

    const addFolder = async (id: DirectoryId, rootPath?: string) => {
      const assetPath = extractTypeAndPath(id).path
      rootPath ??= path.dirname(assetPath)
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
        // These asset types are not present on the Local Backend,
        // however include them to force any newly added asset types to be handled
        // (by causing a non-exhaustiveness error).
        case AssetType.secret:
        case AssetType.datalink: {
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
    request: http.IncomingMessage,
    response: http.ServerResponse,
    params: URLSearchParams,
  ) {
    try {
      // This is SAFE because it is wrapped in a try-catch.
      // If the body is not valid JSON, an error will be thrown and handled.
      const body = await streamConsumers.json(request)
      const assetIds =
        (
          typeof body === 'object' &&
          body &&
          'assetIds' in body &&
          Array.isArray(body.assetIds) &&
          body.assetIds.every((id) => typeof id === 'string')
        ) ?
          body.assetIds.map((id) => AssetId(id))
        : null
      if (!assetIds) {
        this.httpError(response, 'Asset IDs invalid or missing.')
        return
      }
      const filePath = params.get('filePath')
      const archive = this.apiArchiveStream(assetIds)
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
      this.httpOkJson<ExportedArchive>(response, {
        filePath: Path(filePath),
      })
    } catch (error) {
      this.httpError(response, error instanceof Error ? error.message : String(error))
    }
  }

  /** Get details for an asset by its path. */
  apiGetAssetDetailsByPath<Type extends AssetType>({
    type,
    path: assetPath,
  }: {
    type?: Type
    path: Path
  }): AnyAsset<Type> | undefined {
    try {
      // @ts-expect-error This is UNSAFE if `Type` is specified explicitly.
      // If it is inferred, this means `type` is present and the constraint correctly falls back to
      // `AssetType`
      type ??= (() => {
        const assetStat = statSync(assetPath)
        if (assetStat.isDirectory()) {
          const metadata = projectManagement.getMetadata(assetPath)
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
        title: path.basename(assetPath),
        modifiedAt: new Date().toISOString() as DirectoryAsset['modifiedAt'],
        parentId: DirectoryId(`directory-${path.dirname(assetPath)}` as const),
        extension: null,
        permissions: [],
        projectState: null,
        parentsPath: ParentsPath(''),
        virtualParentsPath: VirtualParentsPath(''),
        ensoPath: EnsoPath(String(assetPath)),
      } satisfies Partial<DirectoryAsset>
      switch (type) {
        case AssetType.project: {
          const result: ProjectAsset = {
            ...shared,
            type: AssetType.project,
            id: ProjectId(`project-${assetPath}`),
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
            id: FileId(`file-${assetPath}`),
            extension: path.extname(assetPath),
          }
          // This is SAFE because `type` has been narrowed in the `switch` above.
          return result as AnyAsset<Type>
        }
        case AssetType.directory: {
          const result: DirectoryAsset = {
            ...shared,
            type: AssetType.directory,
            id: DirectoryId(`directory-${assetPath}` as const),
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
      const asset = this.apiGetAssetDetailsByPath({ path: entryPath })
      if (asset == null) {
        throw new Error(`File not found at '${entryPath}'`)
      }
      assets.push(asset)
    }
    return assets
  }

  /** Response handler for "upload file" endpoint. */
  async httpUploadFile(
    request: http.IncomingMessage,
    response: http.ServerResponse,
    params: URLSearchParams,
  ) {
    const directoryParam = params.get('directory') as DirectoryId | null
    const directory =
      directoryParam ? extractTypeAndPath(directoryParam).path : this.projectsRootDirectory
    const fileName = params.get('file_name')
    const filePath = params.get('file_path')
    try {
      if (fileName == null) {
        response
          .writeHead(HTTP_STATUS_BAD_REQUEST, COOP_COEP_CORP_HEADERS)
          .end('Request is missing search parameter `file_name`.')
      } else if (fileNameIsArchive(fileName)) {
        await this.apiUploadArchive({
          directoryId: directoryParam,
          filePath,
          readStream: request,
        })
        response.writeHead(HTTP_STATUS_OK, COOP_COEP_CORP_HEADERS).end()
      } else if (fileNameIsProject(fileName)) {
        const projectName = stripProjectExtension(fileName)
        const project =
          filePath ?
            projectManagement.importProjectFromPath(filePath, directory, projectName)
          : await projectManagement.uploadBundle(request, directory, projectName)
        this.httpOkText(response, project.projectRoot)
      } else {
        const filePath = path.join(directory, fileName)
        void writeFile(filePath, request)
          .then(() => {
            this.httpOkText(response, filePath)
          })
          .catch((e) => {
            console.error(e)
            response.writeHead(HTTP_STATUS_BAD_REQUEST, COOP_COEP_CORP_HEADERS).end()
          })
      }
    } catch (error) {
      response
        .writeHead(HTTP_STATUS_BAD_REQUEST, COOP_COEP_CORP_HEADERS)
        .end(
          String(
            typeof error === 'object' && error != null && 'message' in error ?
              error.message
            : error,
          ),
        )
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
      const result = await handleFilesystemCommand(cliArguments, request)

      if (typeof result === 'string') {
        const resultData = Buffer.from(result)
        response
          .writeHead(HTTP_STATUS_OK, {
            'Content-Length': String(resultData.byteLength),
            'Content-Type': 'application/json',
            ...COOP_COEP_CORP_HEADERS,
          })
          .end(resultData)
      } else {
        const responseWithHead = response.writeHead(HTTP_STATUS_OK, {
          'Content-Type': 'application/octet-stream',
          ...COOP_COEP_CORP_HEADERS,
        })
        result.pipe(responseWithHead, { end: true })
      }
    }
  }
}
