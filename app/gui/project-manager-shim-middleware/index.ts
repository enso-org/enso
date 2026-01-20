/**
 * @file A HTTP server middleware which handles routes normally proxied through to
 * the Project Manager.
 */
import * as fsSync from 'node:fs'
import * as fs from 'node:fs/promises'
import * as http from 'node:http'
import * as https from 'node:https'
import * as path from 'node:path'

import {
  AssetType,
  DirectoryId,
  EnsoPath,
  extractTypeAndPath,
  extractTypeFromId,
  FileId,
  fileNameIsArchive,
  fileNameIsProject,
  ParentsPath,
  Path,
  ProjectId,
  ProjectState,
  stripProjectExtension,
  UnzipAssetsJobId,
  VirtualParentsPath,
  type AnyAsset,
  type AssetId,
  type DirectoryAsset,
  type ExportedArchive,
  type FileAsset,
  type ProjectAsset,
} from 'enso-common/src/services/Backend'
import {
  DOWNLOAD_PROJECT_REGEX,
  EXPORT_ARCHIVE_PATH,
} from 'enso-common/src/services/Backend/remoteBackendPaths'
import { toRfc3339 } from 'enso-common/src/utilities/data/dateTime'
import { tmpdir } from 'node:os'
import type { Readable } from 'node:stream'
import { finished } from 'node:stream/promises'
import { createGzip } from 'node:zlib'
import * as projectManagement from 'project-manager-shim'
import { type Watcher } from 'project-manager-shim/fs'
import {
  handleFilesystemCommand,
  handleProjectServiceRequest,
  handleWatcherRequest,
  isProjectServiceRequest,
  isWatcherRequest,
} from 'project-manager-shim/handler'
import { ProjectService } from 'project-manager-shim/projectService'
import { tarFsPack, unzipEntries, zipWriteStream } from './archive'

// =================
// === Constants ===
// =================

const FS_MAX_RETRIES = 3

const HTTP_STATUS_OK = 200
const HTTP_STATUS_BAD_REQUEST = 400
const HTTP_STATUS_NOT_FOUND = 404
const HTTP_STATUS_INTERNAL_SERVER_ERROR = 500
const PROJECTS_ROOT_DIRECTORY = projectManagement.getProjectsDirectory().replace(/\\/g, '/')

const COMMON_HEADERS = {
  'Cross-Origin-Opener-Policy': 'same-origin',
  'Cross-Origin-Resource-Policy': 'same-origin',
}
const COOP_COEP_CORP_HEADERS = [
  ['Cross-Origin-Opener-Policy', 'same-origin'],
  ['Cross-Origin-Resource-Policy', 'same-origin'],
]

// ====================================
// === ProjectManagerShimMiddleware ===
// ====================================

/** Middleware for project manager shim. */
export class ProjectManagerShimMiddleware {
  private projectService?: ProjectService
  private watchers: Map<AssetId, Watcher> = new Map()

  /** Create the new middleware. */
  constructor(private readonly setup: () => Promise<void>) {}

  /** Get the project service. */
  async getProjectService(): Promise<ProjectService> {
    if (!this.projectService) {
      await this.setup()
      this.projectService = ProjectService.default()
    }
    return this.projectService
  }

  /** A middleware handler.  */
  handler(request: http.IncomingMessage, response: http.ServerResponse, next: () => void) {
    const requestUrl = request.url ?? ''
    if (!requestUrl.startsWith('/api/')) return next()
    const url = new URL(requestUrl, 'https://apishim.local')
    const requestPath = url.pathname
    if (requestUrl != null && requestUrl.startsWith('/api/cloud/')) {
      switch (requestPath) {
        case '/api/cloud/download-project': {
          const downloadUrl = url.searchParams.get('downloadUrl')
          const projectId = url.searchParams.get('projectId')

          if (downloadUrl == null) {
            response
              .writeHead(HTTP_STATUS_BAD_REQUEST, COMMON_HEADERS)
              .end('Request is missing search parameter `downloadUrl`.')
            break
          }

          if (projectId == null) {
            response
              .writeHead(HTTP_STATUS_BAD_REQUEST, COMMON_HEADERS)
              .end('Request is missing search parameter `projectId`.')
            break
          }

          https.get(downloadUrl, (actualResponse) => {
            const projectsDirectory = projectManagement.getProjectsDirectory()
            const parentDirectory = path.join(projectsDirectory, `cloud-${projectId}`)
            const projectRootDirectory = path.join(parentDirectory, 'project_root')

            fs.rm(parentDirectory, { recursive: true, force: true, maxRetries: FS_MAX_RETRIES })
              .then(() => fs.mkdir(projectRootDirectory, { recursive: true }))
              .then(() => projectManagement.unpackBundle(actualResponse, projectRootDirectory))
              .then(() => {
                response
                  .writeHead(HTTP_STATUS_OK, COMMON_HEADERS)
                  .end(JSON.stringify({ parentDirectory, projectRootDirectory }))
              })
              .catch((e) => {
                console.error(e)
                try {
                  if (fsSync.existsSync(parentDirectory)) {
                    fsSync.rmdirSync(parentDirectory, { maxRetries: 3, recursive: true })
                  }
                } catch (e) {
                  console.error(`Failed to cleanup directory ${parentDirectory}.`, e)
                }
                response.writeHead(HTTP_STATUS_INTERNAL_SERVER_ERROR, COMMON_HEADERS).end()
              })
          })

          break
        }
        case '/api/cloud/get-project-archive': {
          const parentDir = url.searchParams.get('directory')

          if (parentDir == null) {
            response
              .writeHead(HTTP_STATUS_BAD_REQUEST, COMMON_HEADERS)
              .end('Request is missing search parameter `directory`.')
            break
          }
          const projectDir = path.join(parentDir, 'project_root')

          projectManagement
            .createBundle(projectDir)
            .then((projectBundle) => {
              response
                .writeHead(HTTP_STATUS_OK, {
                  ...COMMON_HEADERS,
                  'Content-Length': String(projectBundle.byteLength),
                })
                .end(projectBundle)
            })
            .catch((err) => {
              console.error(err)
              response.writeHead(HTTP_STATUS_INTERNAL_SERVER_ERROR, COMMON_HEADERS).end()
            })

          break
        }
        default: {
          console.error(`Unknown Cloud middleware request:`, requestPath)
          break
        }
      }
    } else if (isProjectServiceRequest(requestPath)) {
      handleProjectServiceRequest(
        request,
        response,
        requestPath,
        () => this.getProjectService(),
        COMMON_HEADERS,
      )
    } else if (isWatcherRequest(requestPath)) {
      handleWatcherRequest(request, response, COMMON_HEADERS, this.watchers)
    } else if (requestPath.startsWith('/api/')) {
      switch (`${request.method} ${requestPath}`) {
        case `POST /api/${EXPORT_ARCHIVE_PATH}`: {
          httpDownloadArchive(request, response, url.searchParams)
          break
        }
        case 'POST /api/upload-file': {
          httpUploadFile(request, response, url.searchParams)
          break
        }
        // This endpoint should only be used when accessing the app from the browser.
        // When accessing the app from Electron, the file input event will have the
        // full system path.
        case 'POST /api/upload-project': {
          const directory = url.searchParams.get('directory')
          const name = url.searchParams.get('name')
          void projectManagement
            .uploadBundle(request, directory, name)
            .then(({ id }) => {
              response
                .writeHead(HTTP_STATUS_OK, {
                  'Content-Length': String(id.length),
                  'Content-Type': 'text/plain',
                  ...COMMON_HEADERS,
                })
                .end(id)
            })
            .catch(() => {
              response.writeHead(HTTP_STATUS_BAD_REQUEST, COMMON_HEADERS).end()
            })
          break
        }
        case 'POST /api/run-project-manager-command': {
          const cliArguments: unknown = JSON.parse(url.searchParams.get('cli-arguments') ?? '[]')
          if (
            !Array.isArray(cliArguments) ||
            !cliArguments.every((item): item is string => typeof item === 'string')
          ) {
            response
              .writeHead(HTTP_STATUS_BAD_REQUEST, COMMON_HEADERS)
              .end('Command arguments must be an array of strings.')
          } else {
            void (async () => {
              const result = await handleFilesystemCommand(cliArguments, request)

              if (typeof result === 'string') {
                const resultData = Buffer.from(result)
                response
                  .writeHead(HTTP_STATUS_OK, {
                    'Content-Length': String(resultData.byteLength),
                    'Content-Type': 'application/json',
                    ...COMMON_HEADERS,
                  })
                  .end(resultData)
              } else {
                const responseWithHead = response.writeHead(HTTP_STATUS_OK, {
                  'Content-Type': 'application/octet-stream',
                  ...COMMON_HEADERS,
                })
                result.pipe(responseWithHead, { end: true })
              }
            })()
          }
          break
        }
        case 'GET /api/root-directory-path': {
          response
            .writeHead(HTTP_STATUS_OK, {
              'Content-Length': String(PROJECTS_ROOT_DIRECTORY.length),
              'Content-Type': 'text/plain',
              ...COMMON_HEADERS,
            })
            .end(PROJECTS_ROOT_DIRECTORY)
          break
        }
        default: {
          const route = requestPath.replace('/api/', '/')
          let match: RegExpMatchArray | null = null

          match = route.match(DOWNLOAD_PROJECT_REGEX)
          if (request.method === 'GET' && match?.groups?.['projectId'] != null) {
            const projectId = ProjectId(match.groups['projectId'])
            const projectPath = extractTypeAndPath(projectId).path
            projectManagement
              .createBundle(projectPath)
              .then((projectBundle) => {
                response
                  .writeHead(HTTP_STATUS_OK, {
                    ...COMMON_HEADERS,
                    'Content-Length': String(projectBundle.byteLength),
                    'Content-Type': 'application/octet-stream',
                  })
                  .end(projectBundle)
              })
              .catch((err) => {
                console.error(err)
                response.writeHead(HTTP_STATUS_INTERNAL_SERVER_ERROR, COMMON_HEADERS).end()
              })

            break
          }

          response.writeHead(HTTP_STATUS_NOT_FOUND, COMMON_HEADERS).end()
          break
        }
      }
    } else {
      next()
    }
  }
}

/** Return whether a file exists. */
async function fileExists(path: string) {
  try {
    await fs.stat(path)
    return true
  } catch {
    return false
  }
}

/** Send a HTTP response with a JSON payload. */
function httpOkJson<T = never>(response: http.ServerResponse, body: NoInfer<T>) {
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
function httpOkText(response: http.ServerResponse, content: string) {
  return response
    .writeHead(HTTP_STATUS_OK, [
      ['Content-Length', `${content.length}`],
      ['Content-Type', 'text/plain'],
      ...COOP_COEP_CORP_HEADERS,
    ])
    .end(content)
}

/** Get details for an asset by its path. */
function apiGetAssetDetailsByPath<Type extends AssetType>({
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
      const assetStat = fsSync.statSync(assetPath)
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
      modifiedAt: toRfc3339(new Date()),
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
          id: ProjectId(`project-${encodeURIComponent(assetPath)}`),
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
          id: FileId(`file-${encodeURIComponent(assetPath)}`),
          extension: path.extname(assetPath),
        }
        // This is SAFE because `type` has been narrowed in the `switch` above.
        return result as AnyAsset<Type>
      }
      case AssetType.directory: {
        const result: DirectoryAsset = {
          ...shared,
          type: AssetType.directory,
          id: DirectoryId(`directory-${encodeURIComponent(assetPath)}` as const),
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

/** List a directory. */
async function apiListDirectory(params: { readonly directory?: DirectoryId }) {
  const { directory: directoryRaw } = params
  const directory = directoryRaw ? extractTypeAndPath(directoryRaw).path : PROJECTS_ROOT_DIRECTORY
  const assets: AnyAsset[] = []
  for (const entryName of await fs.readdir(directory)) {
    const entryPath = Path(path.join(directory, entryName))
    const asset = apiGetAssetDetailsByPath({ path: entryPath })
    if (asset == null) {
      throw new Error(`File not found at '${entryPath}'`)
    }
    assets.push(asset)
  }
  return assets
}

/** Create an archive stream with the given assets. */
function apiArchiveStream(assets: readonly AssetId[]) {
  const archive = zipWriteStream()

  const addProject = async (id: ProjectId, rootPath?: string) => {
    const assetPath = extractTypeAndPath(id).path
    rootPath ??= path.dirname(assetPath)
    const pathInArchive = `${path.relative(rootPath, assetPath)}.enso-project`
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
    await archive.addFile(fsSync.createReadStream(assetPath), { name: pathInArchive })
  }

  const addFolder = async (id: DirectoryId, rootPath?: string) => {
    const assetPath = extractTypeAndPath(id).path
    rootPath ??= path.dirname(assetPath)
    const pathInArchive = path.relative(rootPath, assetPath)
    if (!(await fileExists(assetPath))) {
      return { type: 'error', error: 'notFound', id } as const
    }
    await archive.addFolder({ name: pathInArchive })
    const entries = await apiListDirectory({ directory: id })
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
async function httpDownloadArchive(
  _request: http.IncomingMessage,
  response: http.ServerResponse,
  params: URLSearchParams,
) {
  const assets = params.getAll('asset') as AssetId[]
  const filePath = params.get('filePath')
  const archive = apiArchiveStream(assets)
  let promise: Promise<void> | undefined
  if (filePath != null) {
    promise = finished(archive.stream.pipe(fsSync.createWriteStream(filePath)))
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
  httpOkJson<ExportedArchive>(response, {
    filePath: Path(filePath),
  })
}

/** Upload an archive, optionally with a list of conflict resolutions. */
async function apiUploadArchive({
  directoryId,
  jobId,
  filePath,
  readStream,
}: {
  directoryId?: DirectoryId | null | undefined
  jobId?: UnzipAssetsJobId | null | undefined
  filePath?: string | null | undefined
  readStream?: Readable | null | undefined
}): Promise<unknown> {
  filePath ??= jobId != null ? Path(decodeURIComponent(jobId)) : undefined
  const directory = directoryId ? extractTypeAndPath(directoryId).path : PROJECTS_ROOT_DIRECTORY
  let tempDirectory: string | undefined
  if (filePath == null) {
    tempDirectory = await fs.mkdtemp(path.join(tmpdir(), 'enso-'))
    filePath = path.join(tempDirectory, 'archive.zip')
    const writeStream = fsSync.createWriteStream(filePath)
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
    let destinationPathInArchive = path.join(parentPathInArchive, path.basename(entryPathInArchive))
    const extensionRaw = path.extname(destinationPathInArchive)
    const basename = path.basename(entryPathInArchive, extensionRaw)
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
    let destinationPath = Path(path.join(directory, `${basename}${extension}`))
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
    const isProject = entryPathInArchive.endsWith('.enso-project')
    const shared = {
      title: path.basename(destinationPath),
      modifiedAt: toRfc3339(new Date()),
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
        id: DirectoryId(`directory-${encodeURIComponent(destinationPath)}` as const),
      })
      await entry.extract({ rootDirectory: directory, destinationPath })
    } else if (isProject) {
      assets.push({
        ...shared,
        type: AssetType.project,
        id: ProjectId(
          `project-${encodeURIComponent(destinationPath.replace('.enso-project', '/'))}`,
        ),
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
        id: FileId(`file-${encodeURIComponent(destinationPath)}`),
        extension: path.extname(destinationPath),
      })
      await entry.extract({ rootDirectory: directory, destinationPath })
    }
  }
  if (tempDirectory != null) {
    await fs.rm(tempDirectory, { force: true, recursive: true })
  }
  return { assets }
}

/** Response handler for "upload file" endpoint. */
async function httpUploadFile(
  request: http.IncomingMessage,
  response: http.ServerResponse,
  params: URLSearchParams,
) {
  const directoryParam = params.get('directory') as DirectoryId | null
  const directory =
    directoryParam ? extractTypeAndPath(directoryParam).path : PROJECTS_ROOT_DIRECTORY
  const fileName = params.get('file_name')
  const filePath = params.get('file_path')
  try {
    if (fileName == null) {
      response
        .writeHead(HTTP_STATUS_BAD_REQUEST, COOP_COEP_CORP_HEADERS)
        .end('Request is missing search parameter `file_name`.')
    } else if (fileNameIsArchive(fileName)) {
      await apiUploadArchive({
        directoryId: directoryParam,
        filePath,
        readStream: request,
      })
      response.writeHead(HTTP_STATUS_OK, COOP_COEP_CORP_HEADERS).end()
    } else if (fileNameIsProject(fileName)) {
      const project =
        filePath ?
          projectManagement.importProjectFromPath(filePath, directory, fileName)
        : await projectManagement.uploadBundle(request, directory, fileName)
      httpOkText(response, project.projectRoot)
    } else {
      const filePath = path.join(directory, fileName)
      void fs
        .writeFile(filePath, request)
        .then(() => {
          httpOkText(response, filePath)
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
          typeof error === 'object' && error != null && 'message' in error ? error.message : error,
        ),
      )
  }
}
