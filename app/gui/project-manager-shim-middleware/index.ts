/**
 * @file A HTTP server middleware which handles routes normally proxied through to
 * the Project Manager.
 */
import * as crypto from 'node:crypto'
import * as fsSync from 'node:fs'
import * as fs from 'node:fs/promises'
import * as http from 'node:http'
import * as https from 'node:https'
import * as path from 'node:path'

import * as tar from 'tar'
import * as yaml from 'yaml'

import GLOBAL_CONFIG from 'enso-common/src/config.json' with { type: 'json' }

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
import { EXPORT_ARCHIVE_PATH } from 'enso-common/src/services/Backend/remoteBackendPaths'
import { toRfc3339 } from 'enso-common/src/utilities/data/dateTime'
import {
  basenameAndExtension,
  getFileName,
  getFolderPath,
  isFolderPath,
} from 'enso-common/src/utilities/file'
import { tmpdir } from 'node:os'
import type { Readable } from 'node:stream'
import { finished } from 'node:stream/promises'
import { createGzip } from 'node:zlib'
import { tarFsPack, unzipEntries, zipWriteStream } from './archive'
import * as projectManagement from './projectManagement'

// =================
// === Constants ===
// =================

const HTTP_STATUS_OK = 200
const HTTP_STATUS_BAD_REQUEST = 400
const HTTP_STATUS_NOT_FOUND = 404
const HTTP_STATUS_INTERNAL_SERVER_ERROR = 500
const PROJECTS_ROOT_DIRECTORY = projectManagement.getProjectsDirectory()

const COMMON_HEADERS = {
  'Cross-Origin-Opener-Policy': 'same-origin',
  'Cross-Origin-Resource-Policy': 'same-origin',
}
const COOP_COEP_CORP_HEADERS = [
  ['Cross-Origin-Opener-Policy', 'same-origin'],
  ['Cross-Origin-Resource-Policy', 'same-origin'],
]

// =============
// === Types ===
// =============

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

/** The discriminator value for {@link FileSystemEntry}. */
enum FileSystemEntryType {
  DirectoryEntry = 'DirectoryEntry',
  ProjectEntry = 'ProjectEntry',
  FileEntry = 'FileEntry',
}

/** Metadata for a file. */
interface FileEntry {
  readonly type: FileSystemEntryType.FileEntry
  readonly path: string
  readonly attributes: Attributes
}

/** Metadata for a directory. */
interface DirectoryEntry {
  readonly type: FileSystemEntryType.DirectoryEntry
  readonly path: string
  readonly attributes: Attributes
}

/** Metadata for a project. */
interface ProjectEntry {
  readonly type: FileSystemEntryType.ProjectEntry
  readonly path: string
  readonly metadata: ProjectMetadata
  readonly attributes: Attributes
}

// ====================================
// === projectManagerShimMiddleware ===
// ====================================

/** A middleware that handles  */
export default function projectManagerShimMiddleware(
  request: http.IncomingMessage,
  response: http.ServerResponse,
  next: () => void,
) {
  const requestUrl = request.url
  const requestPath = requestUrl?.split('?')[0]?.split('#')[0]
  if (requestUrl != null && requestUrl.startsWith('/api/project-manager/')) {
    const actualUrl = new URL(
      requestUrl.replace(/^\/api\/project-manager/, GLOBAL_CONFIG.projectManagerHttpEndpoint),
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
  } else if (requestUrl != null && requestUrl.startsWith('/api/cloud/')) {
    switch (requestPath) {
      case '/api/cloud/download-project': {
        const url = new URL(`https://example.com/${requestUrl}`)
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

          fs.mkdir(projectRootDirectory, { recursive: true })
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
        const url = new URL(`https://example.com/${requestUrl}`)
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
  } else if (request.method === 'POST') {
    const params = new URL(requestUrl ?? '').searchParams
    switch (requestPath) {
      case `/api/${EXPORT_ARCHIVE_PATH}`: {
        httpDownloadArchive(request, response, params)
        break
      }
      case '/api/upload-file': {
        httpUploadFile(request, response, params)
        break
      }
      // This endpoint should only be used when accessing the app from the browser.
      // When accessing the app from Electron, the file input event will have the
      // full system path.
      case '/api/upload-project': {
        const url = new URL(`https://example.com/${requestUrl}`)
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
      case '/api/run-project-manager-command': {
        const cliArguments: unknown = JSON.parse(
          new URL(`https://example.com/${requestUrl}`).searchParams.get('cli-arguments') ?? '[]',
        )
        if (
          !Array.isArray(cliArguments) ||
          !cliArguments.every((item): item is string => typeof item === 'string')
        ) {
          response
            .writeHead(HTTP_STATUS_BAD_REQUEST, COMMON_HEADERS)
            .end('Command arguments must be an array of strings.')
        } else {
          void (async () => {
            const toJSONRPCResult = (result: unknown) =>
              JSON.stringify({ jsonrpc: '2.0', id: 0, result })
            const toJSONRPCError = (message: string, data?: unknown) =>
              JSON.stringify({
                jsonrpc: '2.0',
                id: 0,
                error: { code: 0, message, ...(data != null ? { data } : {}) },
              })
            let result: string | fsSync.ReadStream = toJSONRPCError(
              `Error running Project Manager command.`,
              {
                command: cliArguments,
              },
            )
            try {
              switch (cliArguments[0]) {
                case '--filesystem-exists': {
                  const directoryPath = cliArguments[1]
                  if (directoryPath != null) {
                    const exists = fsSync.existsSync(directoryPath)
                    result = toJSONRPCResult({ exists })
                  }
                  break
                }
                case '--filesystem-list': {
                  const directoryPath = cliArguments[1]
                  if (directoryPath != null) {
                    const entryNames = await fs.readdir(directoryPath)
                    const entries: FileSystemEntry[] = []
                    for (const entryName of entryNames) {
                      const entryPath = path.join(directoryPath, entryName)
                      if (isHidden(entryPath)) continue
                      const stat = await fs.stat(entryPath)
                      const attributes: Attributes = {
                        byteSize: stat.size,
                        creationTime: new Date(stat.ctimeMs).toISOString(),
                        lastAccessTime: new Date(stat.atimeMs).toISOString(),
                        lastModifiedTime: new Date(stat.mtimeMs).toISOString(),
                      }
                      if (stat.isFile()) {
                        entries.push({
                          type: FileSystemEntryType.FileEntry,
                          path: entryPath,
                          attributes,
                        } satisfies FileEntry)
                      } else {
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
                            if (
                              'name' in packageMetadataYaml &&
                              typeof packageMetadataYaml.name === 'string'
                            ) {
                              projectMetadataJson = {
                                id: crypto.randomUUID(),
                                kind: 'UserProject',
                                created: new Date().toISOString(),
                                lastOpened: null,
                              }
                              await fs.mkdir(path.dirname(projectMetadataPath), { recursive: true })
                              await fs.writeFile(
                                projectMetadataPath,
                                JSON.stringify(projectMetadataJson),
                              )
                            } else {
                              throw e
                            }
                          }
                          const metadata = extractProjectMetadata(
                            packageMetadataYaml,
                            projectMetadataJson,
                          )
                          if (metadata != null) {
                            // This is a project.
                            entries.push({
                              type: FileSystemEntryType.ProjectEntry,
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
                            type: FileSystemEntryType.DirectoryEntry,
                            path: entryPath,
                            attributes,
                          } satisfies DirectoryEntry)
                        }
                      }
                    }
                    result = toJSONRPCResult({ entries })
                  }
                  break
                }
                case '--filesystem-create-directory': {
                  const directoryPath = cliArguments[1]
                  if (directoryPath != null) {
                    await fs.mkdir(directoryPath, { recursive: true })
                    result = toJSONRPCResult(null)
                  }
                  break
                }
                case '--filesystem-read-path': {
                  const filePath = cliArguments[1]
                  if (filePath != null) {
                    result = await fsSync.createReadStream(filePath)
                  }
                  break
                }
                case '--filesystem-write-path': {
                  const filePath = cliArguments[1]
                  if (filePath != null) {
                    await new Promise((resolve, reject) => {
                      request
                        .pipe(fsSync.createWriteStream(filePath), {
                          end: true,
                        })
                        .on('close', resolve)
                        .on('error', reject)
                    })
                    result = toJSONRPCResult(null)
                  }
                  break
                }
                case '--filesystem-move-from': {
                  const sourcePath = cliArguments[1]
                  const destinationPath = cliArguments[3]
                  if (
                    sourcePath != null &&
                    cliArguments[2] === '--filesystem-move-to' &&
                    destinationPath != null
                  ) {
                    await fs.rename(sourcePath, destinationPath)
                    result = toJSONRPCResult(null)
                  }
                  break
                }
                case '--filesystem-delete': {
                  const fileOrDirectoryPath = cliArguments[1]
                  if (fileOrDirectoryPath != null) {
                    await fs.rm(fileOrDirectoryPath, { recursive: true })
                    result = toJSONRPCResult(null)
                  }
                  break
                }
                default: {
                  const message = `Error in Project Manager shim: unknown command ${JSON.stringify(cliArguments)}`
                  console.error(message)
                  result = toJSONRPCError(message)
                  break
                }
              }
            } catch {
              // Ignored. `result` retains its original value indicating an error.
            }

            const resultData = typeof result === 'string' ? Buffer.from(result) : result
            if (resultData instanceof fsSync.ReadStream) {
              const responseWithHead = response.writeHead(HTTP_STATUS_OK, {
                'Content-Type': 'application/octet-stream',
                ...COMMON_HEADERS,
              })
              resultData.pipe(responseWithHead)
            } else {
              response
                .writeHead(HTTP_STATUS_OK, {
                  'Content-Length': String(resultData.byteLength),
                  'Content-Type': 'application/json',
                  ...COMMON_HEADERS,
                })
                .end(resultData)
            }
          })()
        }
        break
      }
      default: {
        const downloadProjectMatch = requestPath?.match(
          /^[/]api[/]project-manager[/]projects[/]([^/]+)[/]enso-project$/,
        )
        if (downloadProjectMatch) {
          const uuid = downloadProjectMatch[1]
          void fs.readdir(PROJECTS_ROOT_DIRECTORY).then(async (filenames) => {
            let success = false
            for (const filename of filenames) {
              try {
                const projectRoot = path.join(PROJECTS_ROOT_DIRECTORY, filename)
                const stat = await fs.stat(projectRoot)
                if (stat.isDirectory()) {
                  const metadataPath = path.join(
                    projectRoot,
                    projectManagement.PROJECT_METADATA_RELATIVE_PATH,
                  )
                  const metadataContents = await fs.readFile(metadataPath)
                  const metadata: unknown = JSON.parse(metadataContents.toString())
                  if (
                    typeof metadata === 'object' &&
                    metadata != null &&
                    'id' in metadata &&
                    metadata.id === uuid
                  ) {
                    response.writeHead(HTTP_STATUS_OK, {
                      'Content-Type': 'application/gzip+x-enso-project',
                      ...COMMON_HEADERS,
                    })
                    tar
                      .create({ gzip: true, cwd: projectRoot }, [projectRoot])
                      .pipe(response, { end: true })
                    success = true
                    break
                  }
                }
              } catch {
                // Ignored.
              }
            }
            if (!success) {
              response.writeHead(HTTP_STATUS_NOT_FOUND, COMMON_HEADERS).end()
            }
          })
          break
        }
        response.writeHead(HTTP_STATUS_NOT_FOUND, COMMON_HEADERS).end()
        break
      }
    }
  } else if (request.method === 'GET' && requestPath === '/api/root-directory-path') {
    response
      .writeHead(HTTP_STATUS_OK, {
        'Content-Length': String(PROJECTS_ROOT_DIRECTORY.length),
        'Content-Type': 'text/plain',
        ...COMMON_HEADERS,
      })
      .end(PROJECTS_ROOT_DIRECTORY)
  } else {
    next()
  }
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

/**
 * Checks if files that start with the dot.
 * Note on Windows does not check the hidden property.
 */
function isHidden(filePath: string): boolean {
  const dotfile = /(^|[\\/])\.[^\\/]+$/g
  return dotfile.test(filePath)
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
      const assetStat = fsSync.statSync(path)
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
      ensoPath: EnsoPath(String(path)),
    } satisfies Partial<DirectoryAsset>
    switch (type) {
      case AssetType.project: {
        const result: ProjectAsset = {
          ...shared,
          type: AssetType.project,
          id: ProjectId(`project-${encodeURIComponent(path)}`),
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
          id: FileId(`file-${encodeURIComponent(path)}`),
          extension: basenameAndExtension(path).extension,
        }
        // This is SAFE because `type` has been narrowed in the `switch` above.
        return result as AnyAsset<Type>
      }
      case AssetType.directory: {
        const result: DirectoryAsset = {
          ...shared,
          type: AssetType.directory,
          id: DirectoryId(`directory-${encodeURIComponent(path)}` as const),
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
    rootPath ??= getFolderPath(assetPath)
    const pathInArchive = `${path.relative(rootPath, assetPath)}.enso-project`
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
    await archive.addFile(fsSync.createReadStream(assetPath), { name: pathInArchive })
  }

  const addFolder = async (id: DirectoryId, rootPath?: string) => {
    const assetPath = extractTypeAndPath(id).path
    rootPath ??= getFolderPath(assetPath)
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
      // These asset types are not valid, however include them to force any newly added
      // asset types to be handled (by causing a non-exhaustiveness error).
      case AssetType.secret:
      case AssetType.datalink:
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
    const isDirectory = isFolderPath(entryPathInArchive)
    const parentPathInArchiveRaw = getFolderPath(entryPathInArchive)
    let parentPathInArchive =
      parentPathInArchiveRaw === entryPathInArchive ? '' : pathMapping[parentPathInArchiveRaw]
    if (parentPathInArchive == null) {
      await getDirectoryPath(parentPathInArchiveRaw)
      parentPathInArchive = pathMapping[parentPathInArchiveRaw] ?? ''
    }
    let destinationPathInArchive = path.join(parentPathInArchive, getFileName(entryPathInArchive))
    const { basename, extension: extensionRaw } = basenameAndExtension(
      getFileName(entryPathInArchive),
    )
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
    const isDirectory = isFolderPath(entryPathInArchive)
    const isProject = entryPathInArchive.endsWith('.enso-project')
    const shared = {
      title: getFileName(destinationPath),
      modifiedAt: toRfc3339(new Date()),
      parentId: DirectoryId(`directory-${getFolderPath(destinationPath)}` as const),
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
          const parentDirectory = getFolderPath(destinationPath)
          const fileName = getFileName(destinationPath)
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
        extension: basenameAndExtension(destinationPath).extension,
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
      httpOkText(response, project.path)
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
