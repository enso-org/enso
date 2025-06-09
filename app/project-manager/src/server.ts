/** @file A simple HTTP server which serves application data to the Electron web-view. */
import { COOP_COEP_CORP_HEADERS } from 'enso-common'
import {
  AnyAsset,
  AssetConflict,
  AssetId,
  AssetType,
  CreateDirectoryRequestBody,
  DirectoryAsset,
  DirectoryId,
  FileDetails,
  FileId,
  FilterBy,
  GetProjectByUuidParams,
  ListDirectoryRequestParams,
  ListDirectoryResponseBody,
  ParentsPath,
  Path,
  prettifyError,
  ProjectAsset,
  ProjectId,
  ProjectState,
  S3FilePath,
  UpdateAssetRequestBody,
  UpdatedDirectory,
  UpdateDirectoryRequestBody,
  VirtualParentsPath,
  type CreatedDirectory,
  type ImportArchiveResponse,
} from 'enso-common/src/services/Backend'
import {
  CLOUD_DOWNLOAD_PROJECT_PATH,
  CLOUD_GET_PROJECT_ARCHIVE_PATH,
  CREATE_DIRECTORY_PATH,
  DELETE_ASSET_REGEX,
  DOWNLOAD_FILE_REGEX,
  DOWNLOAD_PROJECT_REGEX,
  downloadFilePath,
  EXPORT_ARCHIVE_PATH,
  FILE_EXISTS_REGEX,
  GET_DOWNLOAD_DIRECTORY_PATH,
  GET_FILE_DETAILS_REGEX,
  GET_PROJECT_BY_UUID_PATH,
  GET_PROJECT_CONTENT_REGEX,
  GET_PROJECT_METADATA_REGEX,
  GET_ROOT_DIRECTORY_PATH,
  IMPORT_ARCHIVE_PATH,
  LIST_DIRECTORY_PATH,
  LOCAL_UPLOAD_FILE_PATH,
  LOCAL_UPLOAD_PROJECT_PATH,
  RUN_PROJECT_MANAGER_COMMAND_PATH,
  UPDATE_DIRECTORY_REGEX,
} from 'enso-common/src/services/Backend/paths'
import { HttpMethod } from 'enso-common/src/services/HttpClient'
import { toRfc3339 } from 'enso-common/src/utilities/data/dateTime'
import { basenameAndExtension, getFileName, getFolderPath } from 'enso-common/src/utilities/file'
import { createReadStream, createWriteStream, statSync } from 'node:fs'
import {
  access,
  mkdir,
  mkdtemp,
  readdir,
  rename,
  rm,
  rmdir,
  stat,
  writeFile,
} from 'node:fs/promises'
import * as http from 'node:http'
import * as https from 'node:https'
import { tmpdir } from 'node:os'
import * as path from 'node:path'
import * as stream from 'node:stream'
import { json } from 'node:stream/consumers'
import { finished } from 'node:stream/promises'
import { createGzip } from 'node:zlib'
import {
  apiArchiveStream,
  apiCreateDirectory,
  apiFileExists,
  apiGetAssetDetailsByPath,
  apiGetProjectByUuid,
  apiGetProjectContent,
  apiGetProjectMetadata,
  apiListDirectory,
  apiUpdateAsset,
} from './api'
import { tarFsPack, tarGzReadStreamToFs, unzipEntries } from './archive'
import { BUNDLED_PROJECT_SUFFIX } from './fileAssociations'
import * as projectManagement from './projectManagement'

// =================
// === Constants ===
// =================

const HTTP_STATUS_OK = 200
const HTTP_STATUS_BAD_REQUEST = 400
const HTTP_STATUS_NOT_FOUND = 404
const HTTP_STATUS_INTERNAL_SERVER_ERROR = 500

const DOWNLOAD_HEADER = ['Content-Disposition', 'attachment']

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

// ==============
// === result ===
// ==============

function result<T extends { readonly type: 'success' | 'error' }>(value: T) {
  return value
}

// ======================
// === newDirectoryId ===
// ======================

/** Create a {@link backend.DirectoryId} from a path. */
export function newDirectoryId(path: Path) {
  return DirectoryId(`directory-${encodeURIComponent(path)}` as const)
}

// ====================
// === newProjectId ===
// ====================

/** Create a {@link backend.ProjectId} from a path. */
export function newProjectId(path: Path) {
  return ProjectId(`project-${encodeURIComponent(path)}`)
}

// =================
// === newFileId ===
// =================

/** Create a {@link backend.FileId} from a path. */
export function newFileId(path: Path) {
  return FileId(`file-${encodeURIComponent(path)}`)
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
// === handle ===
// ==============

/**
 * Respond to an incoming request.
 * @throws {Error} when passing invalid JSON to
 * `/api/run-project-manager-command?cli-arguments=<urlencoded-json>`.
 */
export async function handle(
  projectsRootDirectory: Path,
  projectsRootDirectoryId: DirectoryId,
  request: http.IncomingMessage,
  response: http.ServerResponse,
) {
  const requestUrl = request.url
  const requestPath = requestUrl?.split('?')[0]?.split('#')[0]
  if (requestUrl == null) {
    logger.error('Request URL is null.')
  } else if (requestUrl.startsWith('/api/cloud/')) {
    const route = new URL(`https://example.com${requestUrl.replace('/api/', '/')}`)
    const params = route.searchParams
    switch (route.pathname) {
      case '/cloud/download-project': {
        await httpCloudDownloadProject(request, response, params)
        break
      }
      case '/cloud/get-project-archive': {
        await httpCloudGetProjectArchive(request, response, params)
        break
      }
      default: {
        logger.error(`Unknown Cloud middleware request:`, requestPath)
        break
      }
    }
  } else if (request.url?.startsWith('/api/')) {
    const route = new URL(`https://example.com${requestUrl.replace('/api/', '/')}`)
    const params = route.searchParams
    switch (`${request.method} ${route.pathname}`) {
      case `GET /${CLOUD_DOWNLOAD_PROJECT_PATH}`: {
        await httpCloudDownloadProject(request, response, params)
        break
      }
      case `GET /${CLOUD_GET_PROJECT_ARCHIVE_PATH}`: {
        await httpCloudGetProjectArchive(request, response, params)
        break
      }
      case `GET /${LIST_DIRECTORY_PATH}`: {
        await httpListDirectory(request, response, params)
        break
      }
      case `GET /${GET_ROOT_DIRECTORY_PATH}`: {
        await httpGetRootDirectory(request, response, params)
        break
      }
      case `GET /${GET_DOWNLOAD_DIRECTORY_PATH}`: {
        await httpGetDownloadDirectory(request, response, params)
        break
      }
      case `POST /${CREATE_DIRECTORY_PATH}`: {
        await httpCreateDirectory(request, response, params)
        break
      }
      case `POST /${EXPORT_ARCHIVE_PATH}`: {
        await httpDownloadArchive(request, response, params)
        break
      }
      case `POST /${IMPORT_ARCHIVE_PATH}`: {
        await httpUploadArchive(request, response, params)
        break
      }
      case `POST /${LOCAL_UPLOAD_FILE_PATH}`: {
        await httpUploadFile(request, response, params)
        break
      }
      case `POST /${LOCAL_UPLOAD_PROJECT_PATH}`: {
        // This endpoint should only be used when accessing the app from the browser.
        // When accessing the app from Electron, the file input event will have the
        // full system path.
        await httpUploadProject(request, response, params)
        break
      }
      case `POST /${RUN_PROJECT_MANAGER_COMMAND_PATH}`: {
        await httpRunProjectManagerCommand(request, response, params)
        break
      }
      case `GET /${GET_PROJECT_BY_UUID_PATH}`: {
        await httpGetProjectByUuid(request, response, params)
        break
      }
      default: {
        let match: RegExpMatchArray | null = null
        match = route.pathname.match(GET_FILE_DETAILS_REGEX)
        if (match?.groups?.['fileId'] != null && request.method === 'GET') {
          const fileId = match.groups['fileId']
          await httpGetFileDetails(request, response, params, [fileId as FileId])
          break
        }
        match = route.pathname.match(DOWNLOAD_FILE_REGEX)
        if (match?.groups?.['fileId'] != null && request.method === 'GET') {
          const fileId = match.groups['fileId']
          await httpDownloadFile(request, response, params, [fileId as FileId])
          break
        }
        match = route.pathname.match(GET_PROJECT_METADATA_REGEX)
        if (match?.groups?.['projectId'] != null && request.method === 'GET') {
          const projectId = match.groups['projectId']
          await httpGetProjectMetadata(request, response, params, [projectId as ProjectId])
          break
        }
        match = route.pathname.match(GET_PROJECT_CONTENT_REGEX)
        if (match?.groups?.['projectId'] != null && request.method === 'GET') {
          const projectId = match.groups['projectId']
          await httpGetProjectContent(request, response, params, [projectId as ProjectId])
          break
        }
        match = route.pathname.match(DOWNLOAD_PROJECT_REGEX)
        if (match?.groups?.['projectId'] != null && request.method === 'GET') {
          const projectId = match.groups['projectId']
          await httpDownloadProject(request, response, params, [projectId as ProjectId])
          break
        }
        match = route.pathname.match(UPDATE_DIRECTORY_REGEX)
        if (match?.groups?.['directoryId'] != null && request.method === 'HEAD') {
          const directoryId = match.groups['directoryId']
          await httpUpdateDirectory(request, response, params, [directoryId as DirectoryId])
          break
        }
        match = route.pathname.match(FILE_EXISTS_REGEX)
        if (match?.groups?.['fileId'] != null && request.method === 'HEAD') {
          const fileId = match.groups['fileId']
          await httpFileExists(request, response, params, [fileId as FileId])
          break
        }
        match = route.pathname.match(DELETE_ASSET_REGEX)
        if (match?.groups?.['assetId'] != null && request.method === 'DELETE') {
          const assetId = match.groups['assetId']
          await httpDeleteAsset(request, response, params, [assetId as AssetId])
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
  }
}

/** Return a parameter if it exists, return an error if it does not. */
function expectMethod(
  request: http.IncomingMessage,
  response: http.ServerResponse,
  method: HttpMethod,
) {
  if (request.method !== method) {
    httpError(response, `Expected HTTP method '${method}', got '${request.method}'.`)
    return false
  }
  return true
}

/** Return a parameter if it exists, return an error if it does not. */
function expectParameter(
  response: http.ServerResponse,
  params: URLSearchParams,
  parameter: string,
) {
  const value = params.get(parameter)
  if (value == null) {
    httpError(response, `Request is missing search parameter '${parameter}'.`)
  }
  return value
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

/** Send a HTTP response with a plain text payload. */
function httpOkPlaintext(response: http.ServerResponse, body: string) {
  const content = body
  return response
    .writeHead(HTTP_STATUS_OK, [
      ['Content-Length', `${content.length}`],
      ['Content-Type', 'text/plain'],
      ...COOP_COEP_CORP_HEADERS,
    ])
    .end(content)
}

/** Send a HTTP response with a {@link stream.Readable} stream payload. */
async function httpOkStream(
  response: http.ServerResponse,
  stream: stream.Readable,
  { mimeType = 'application/octet-stream', download = false } = {},
) {
  response.writeHead(HTTP_STATUS_OK, [
    ['Content-Type', mimeType],
    ...(download ? [DOWNLOAD_HEADER] : []),
    ...COOP_COEP_CORP_HEADERS,
  ])
  await finished(stream.pipe(response))
}

/** Send a failing HTTP response with an error payload. */
function httpError(response: http.ServerResponse, message: string) {
  return response
    .writeHead(HTTP_STATUS_BAD_REQUEST, [
      ['Content-Length', `${message.length}`],
      ['Content-Type', 'text/plain'],
      ...COOP_COEP_CORP_HEADERS,
    ])
    .end(message)
}

/** HTTP response handler for "download project from cloud" endpoint. */
async function apiCloudDownloadProject(downloadUrl: string, projectId: ProjectId) {
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

/** HTTP response handler for "download project from cloud" endpoint. */
async function httpCloudDownloadProject(
  _request: http.IncomingMessage,
  response: http.ServerResponse,
  params: URLSearchParams,
) {
  const downloadUrl = expectParameter(response, params, 'downloadUrl')
  const projectId = expectParameter(response, params, 'projectId')
  if (downloadUrl == null || projectId == null) {
    return
  }

  try {
    httpOkJson<{
      readonly targetDirectory: string
      readonly parentDirectory: string
    }>(response, await apiCloudDownloadProject(downloadUrl, projectId as ProjectId))
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

/** HTTP response handler for "create directory" endpoint. */
async function httpCreateDirectory(
  request: http.IncomingMessage,
  response: http.ServerResponse,
  _params: URLSearchParams,
) {
  const body = await json(request)
  const parsed = CreateDirectoryRequestBody.safeParse(body)
  if (!parsed.success) {
    httpError(response, prettifyError(parsed.error))
    return
  }
  httpOkJson<CreatedDirectory>(response, await apiCreateDirectory(parsed.data))
}

/** HTTP response handler for "create directory" endpoint. */
async function httpUpdateAsset(
  request: http.IncomingMessage,
  response: http.ServerResponse,
  _params: URLSearchParams,
  [assetId]: [assetId: AssetId],
) {
  const body = await json(request)
  const parsed = UpdateAssetRequestBody.safeParse(body)
  if (!parsed.success) {
    httpError(response, prettifyError(parsed.error))
    return
  }
  const result = await apiUpdateAsset({ id: assetId, ...parsed.data })
  if (result.type === 'success') {
    httpOkJson<null>(response, result.data)
  } else {
    httpError(response, result.message)
  }
}

/** HTTP response handler for "get project archive for cloud" endpoint. */
async function httpCloudGetProjectArchive(
  _request: http.IncomingMessage,
  response: http.ServerResponse,
  params: URLSearchParams,
) {
  const projectDir = expectParameter(response, params, 'directory')
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

/** HTTP response handler for "get file details" endpoint. */
async function apiGetFileDetails(fileId: FileId) {
  const typeAndPath = extractTypeAndPath(fileId)
  const { path: filePath } = typeAndPath
  const file = apiGetAssetDetailsByPath(typeAndPath)
  if (file == null) {
    return
  }
  const stat = statSync(filePath)
  const result: FileDetails = {
    file: {
      fileId,
      fileName: getFileName(filePath),
      // Incorrect, but not sure what to do.
      path: S3FilePath(String(filePath)),
    },
    metadata: { size: stat.size },
    url: downloadFilePath(fileId),
  }
  return result
}

/** HTTP response handler for "get file details" endpoint. */
async function httpGetFileDetails(
  _request: http.IncomingMessage,
  response: http.ServerResponse,
  _params: URLSearchParams,
  [fileId]: [fileId: FileId],
) {
  const details = await apiGetFileDetails(fileId)
  if (!details) {
    const filePath = extractTypeAndPath(fileId).path
    httpError(response, `File '${filePath}' not found`)
    return
  }
  httpOkJson<FileDetails>(response, details)
}

/** HTTP response handler for "download file" endpoint. */
async function httpDownloadFile(
  _request: http.IncomingMessage,
  response: http.ServerResponse,
  _params: URLSearchParams,
  [fileId]: [fileId: FileId],
) {
  const filePath = extractTypeAndPath(fileId).path
  httpOkStream(response, createReadStream(filePath), { download: true })
}

/** HTTP response handler for "get project metadata" endpoint. */
async function httpGetProjectMetadata(
  _request: http.IncomingMessage,
  response: http.ServerResponse,
  _params: URLSearchParams,
  [projectId]: [projectId: ProjectId],
) {
  const metadata = apiGetProjectMetadata({ projectId })
  if (metadata) {
    httpOkJson<typeof metadata>(response, metadata)
  } else {
    const projectPath = extractTypeAndPath(projectId).path
    httpError(response, `Could not get metadata of project at '${projectPath}'`)
  }
}

/** HTTP response handler for "get project by uuid" endpoint. */
async function httpGetProjectByUuid(
  _request: http.IncomingMessage,
  response: http.ServerResponse,
  params: URLSearchParams,
) {
  const parsed = GetProjectByUuidParams.safeParse({
    uuid: params.get('uuid'),
    directoryId: params.get('directory_id'),
  })
  if (!parsed.success) {
    httpError(response, prettifyError(parsed.error))
    return
  }
  const { uuid, directoryId } = parsed.data
  const asset = await apiGetProjectByUuid({ uuid, directoryId })
  if (!asset) {
    const directoryPath = extractTypeAndPath(directoryId).path
    httpError(response, `Could not find project with UUID '${uuid}' in '${directoryPath}'`)
  } else {
    httpOkJson<ProjectAsset>(response, asset)
  }
}

/** HTTP response handler for "get project content" endpoint. */
async function httpGetProjectContent(
  _request: http.IncomingMessage,
  response: http.ServerResponse,
  _params: URLSearchParams,
  [projectId]: [projectId: ProjectId],
) {
  try {
    httpOkStream(response, apiGetProjectContent({ projectId }))
  } catch {
    const projectPath = extractTypeAndPath(projectId).path
    httpError(response, `Could not find main file in project at '${projectPath}'`)
  }
}

/** HTTP response handler for "download project" endpoint. */
async function httpDownloadProject(
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
    promise = httpOkStream(response, stream, { download: true })
  }
  if (filePath == null) {
    return
  }
  await promise
  httpOkJson<null>(response, null)
}

/** Update a directory. */
async function apiUpdateDirectory({
  directoryId,
  title,
}: UpdateDirectoryRequestBody & { readonly directoryId: DirectoryId }) {
  const directoryPath = extractTypeAndPath(directoryId).path
  const parentDirectory = Path(getFolderPath(directoryPath))
  const updateAssetResult = await apiUpdateAsset({
    id: directoryId,
    parentDirectoryId: null,
    description: null,
    title,
  })
  if (updateAssetResult.type === 'error') {
    return updateAssetResult
  }
  const newPath = Path(path.join(parentDirectory, title))
  const data: UpdatedDirectory = {
    id: newDirectoryId(newPath),
    parentId: newDirectoryId(parentDirectory),
    title,
  }
  return result({ type: 'success', data })
}

/** HTTP response handler for "update directory" endpoint. */
async function httpUpdateDirectory(
  request: http.IncomingMessage,
  response: http.ServerResponse,
  _params: URLSearchParams,
  [directoryId]: [directoryId: DirectoryId],
) {
  const body = await json(request)
  const parsed = UpdateDirectoryRequestBody.safeParse(body)
  if (!parsed.success) {
    httpError(response, prettifyError(parsed.error))
    return
  }
  const data = await apiUpdateDirectory({ directoryId, ...parsed.data })
  if (data.type === 'error') {
    httpError(response, data.message)
    return
  }
  httpOkJson<UpdatedDirectory>(response, data.data)
}

/** HTTP response handler for "file exists" endpoint. */
async function httpFileExists(
  _request: http.IncomingMessage,
  response: http.ServerResponse,
  _params: URLSearchParams,
  [fileId]: [fileId: FileId],
) {
  if (await apiFileExists({ fileId })) {
    response.writeHead(HTTP_STATUS_OK).end()
  } else {
    response.writeHead(HTTP_STATUS_NOT_FOUND).end()
  }
}

/** HTTP response handler for "delete asset" endpoint. */
async function httpDeleteAsset(
  _request: http.IncomingMessage,
  response: http.ServerResponse,
  _params: URLSearchParams,
  [assetId]: [assetId: AssetId],
) {
  const assetPath = extractTypeAndPath(assetId).path
  try {
    response.writeHead(HTTP_STATUS_OK).end()
    await rm(assetPath)
  } catch {
    httpError(response, `Could not delete '${assetPath}' because the file does not exist`)
  }
}

/** HTTP response handler for "download archive" endpoint. */
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
    promise = finished(archive.stream.pipe(createWriteStream(filePath)))
  } else {
    promise = httpOkStream(response, archive.stream, { download: true })
  }

  if (filePath == null) {
    // The HTTP headers were already sent
    return
  }
  const error = await archive.promise
  if (error) {
    httpError(response, error.message)
    return
  }
  await promise
  httpOkJson<null>(response, null)
}

/** HTTP response handler for "list directory" endpoint. */
async function httpListDirectory(
  _request: http.IncomingMessage,
  response: http.ServerResponse,
  params: URLSearchParams,
) {
  const parsed = ListDirectoryRequestParams.safeParse({
    recentProjects: params.get('recent_projects') === String(true),
    parentId: params.get('parent_id'),
    filterBy: params.get('filter_by'),
    labels: params.getAll('label'),
  })
  if (!parsed.success) {
    httpError(response, prettifyError(parsed.error))
    return
  }
  const { recentProjects, filterBy, labels, parentId, rootPath } = parsed.data
  const assets =
    recentProjects || filterBy !== FilterBy.active || (labels?.length ?? 0) !== 0 ?
      []
    : await apiListDirectory({ directoryId: parentId, rootPath, projectsRootDirectory })
  return httpOkJson<ListDirectoryResponseBody>(response, { assets })
}

/** HTTP response handler for "upload archive" endpoint. */
async function httpUploadArchive(
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
        sourcePath: Path(entryPathInArchive),
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
            while (await fileExists(path.join(destinationPath, originalSingleChild, singleChild))) {
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
  }
  if (tempDirectory != null) {
    await rm(tempDirectory, { force: true, recursive: true })
  }
  httpOkJson<ImportArchiveResponse>(response, conflicts.length === 0 ? { assets } : { conflicts })
}

/** HTTP response handler for "upload file" endpoint. */
async function httpUploadFile(
  request: http.IncomingMessage,
  response: http.ServerResponse,
  params: URLSearchParams,
) {
  const fileName = expectParameter(response, params, 'file_name')
  if (fileName == null) {
    return
  }

  const directoryParam = params.get('directory') as DirectoryId | null
  const directory =
    directoryParam ? extractTypeAndPath(directoryParam).path : this.projectsRootDirectory

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

/** HTTP response handler for "upload project" endpoint. */
async function httpUploadProject(
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

/** HTTP response handler for "run project manager command" endpoint. */
async function httpRunProjectManagerCommand(
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
    httpOkStream(response, commandOutput)
  }
}
