/**
 * @file A HTTP server middleware which handles routes normally proxied through to
 * the Project Manager.
 */
import * as fsSync from 'node:fs'
import * as fs from 'node:fs/promises'
import * as http from 'node:http'
import * as path from 'node:path'

import * as tar from 'tar'
import * as yaml from 'yaml'

import * as common from 'enso-common'
import GLOBAL_CONFIG from 'enso-common/src/config.json' assert { type: 'json' }

import * as projectManagement from './projectManagement'

// =================
// === Constants ===
// =================

const HTTP_STATUS_OK = 200
const HTTP_STATUS_BAD_REQUEST = 400
const HTTP_STATUS_NOT_FOUND = 404
const PROJECTS_ROOT_DIRECTORY = projectManagement.getProjectsDirectory()

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
export enum FileSystemEntryType {
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
  } else if (request.method === 'POST') {
    switch (requestPath) {
      case '/api/upload-file': {
        const url = new URL(`https://example.com/${requestUrl}`)
        const fileName = url.searchParams.get('file_name')
        const directory = url.searchParams.get('directory') ?? PROJECTS_ROOT_DIRECTORY
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
            .catch((e) => {
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
        void projectManagement
          .uploadBundle(request, directory, name)
          .then((id) => {
            response
              .writeHead(HTTP_STATUS_OK, [
                ['Content-Length', String(id.length)],
                ['Content-Type', 'text/plain'],
                ...common.COOP_COEP_CORP_HEADERS,
              ])
              .end(id)
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
          void (async () => {
            const toJSONRPCResult = (result: unknown) =>
              JSON.stringify({ jsonrpc: '2.0', id: 0, result })
            const toJSONRPCError = (message: string, data?: unknown) =>
              JSON.stringify({
                jsonrpc: '2.0',
                id: 0,
                error: { code: 0, message, ...(data != null ? { data } : {}) },
              })
            let result = toJSONRPCError(`Error running Project Manager command.`, {
              command: cliArguments,
            })
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
                          const projectMetadataContents = await fs.readFile(projectMetadataPath)
                          const metadata = extractProjectMetadata(
                            yaml.parse(packageMetadataContents.toString()),
                            JSON.parse(projectMetadataContents.toString()),
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
            const buffer = Buffer.from(result)
            response
              .writeHead(HTTP_STATUS_OK, [
                ['Content-Length', String(buffer.byteLength)],
                ['Content-Type', 'application/json'],
                ...common.COOP_COEP_CORP_HEADERS,
              ])
              .end(buffer)
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
                    response.writeHead(HTTP_STATUS_OK, [
                      ['Content-Type', 'application/gzip+x-enso-project'],
                      ...common.COOP_COEP_CORP_HEADERS,
                    ])
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
              response.writeHead(HTTP_STATUS_NOT_FOUND, common.COOP_COEP_CORP_HEADERS).end()
            }
          })
          break
        }
        response.writeHead(HTTP_STATUS_NOT_FOUND, common.COOP_COEP_CORP_HEADERS).end()
        break
      }
    }
  } else if (request.method === 'GET' && requestPath === '/api/root-directory') {
    response
      .writeHead(HTTP_STATUS_OK, [
        ['Content-Length', String(PROJECTS_ROOT_DIRECTORY.length)],
        ['Content-Type', 'text/plain'],
        ...common.COOP_COEP_CORP_HEADERS,
      ])
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
