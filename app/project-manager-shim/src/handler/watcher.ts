import { AssetId, DirectoryId, ProjectId } from 'enso-common/src/services/Backend'
import { HttpClient } from 'enso-common/src/services/HttpClient'
import { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import { extractIdFromDirectoryId } from 'enso-common/src/services/RemoteBackend/ids'
import { getText, resolveDictionary, type DefaultGetText } from 'enso-common/src/text'
import { mkdir, rm } from 'node:fs/promises'
import type * as http from 'node:http'
import * as https from 'node:https'
import path from 'node:path'
import { watch, type Watcher } from '../fs.js'
import * as projectManagement from '../projectManagement.js'
import { uploadFile } from '../upload.js'
import { bodyJson } from './http.js'

// =================
// === Constants ===
// =================

const HTTP_STATUS_OK = 200
const HTTP_STATUS_IS_DIRTY = 201
const HTTP_STATUS_BAD_REQUEST = 400
const HTTP_STATUS_NOT_FOUND = 404
const HTTP_STATUS_ERROR = 500

const PROJECT_WATCHER_CALLBACK_DELAY = 10000
const PROJECT_WATCHER_CALLBACK_TIMEOUT = 60000

// ===========================
// === createRemoteBackend ===
// ===========================

/** Create a RemoteBackend instance. */
function createRemoteBackend(headers: Record<string, string>, apiUrl: string): RemoteBackend {
  const client = new HttpClient(headers)
  const downloader = () => {
    // not required for watcher
  }
  const dictionary = resolveDictionary()
  const backendGetText: DefaultGetText = function (key, ...replacements) {
    return getText(dictionary, key, ...replacements)
  }
  return new RemoteBackend({
    apiUrl,
    getText: backendGetText,
    client,
    downloader,
    downloadCloudProject: async (params) => {
      const response = await new Promise<http.IncomingMessage>((resolve) =>
        https.get(params.downloadUrl, resolve),
      )
      const projectsDirectory = projectManagement.getProjectsDirectory()
      const parentDirectory = path.join(projectsDirectory, `cloud-${params.projectId}`)
      const projectRootDirectory = path.join(parentDirectory, 'project_root')

      await rm(parentDirectory, { recursive: true, force: true, maxRetries: 3 })
      await mkdir(projectRootDirectory, { recursive: true })
      await projectManagement.unpackBundle(response, projectRootDirectory)
      return { projectRootDirectory, parentDirectory }
    },
    getProjectArchive: async (directoryId, fileName) => {
      const parentDir = extractIdFromDirectoryId(directoryId)
      const projectDir = path.join(parentDir, 'project_root')
      const projectBundle = await projectManagement.createBundle(projectDir)
      return new File([projectBundle], fileName)
    },
  })
}

// ============================
// === handleWatcherRequest ===
// ============================

/** Check if this is a watcher request. */
export function isWatcherRequest(requestPath: string): boolean {
  return requestPath.startsWith('/api/watcher/')
}

/** Handle watcher requests. */
export async function handleWatcherRequest(
  request: http.IncomingMessage,
  response: http.ServerResponse,
  headers: Record<string, string>,
  watchers: Map<AssetId, Watcher>,
): Promise<void> {
  const url = new URL(request.url ?? '', 'https://apishim.local')
  const requestPath = url.pathname
  switch (`${request.method} ${requestPath}`) {
    case 'POST /api/watcher/start': {
      const projectDir = url.searchParams.get('directory')
      if (projectDir == null) {
        response
          .writeHead(HTTP_STATUS_BAD_REQUEST, headers)
          .end('Request is missing search parameter `directory`.')
        break
      }
      const assetIdString = url.searchParams.get('assetId')
      if (assetIdString == null) {
        response
          .writeHead(HTTP_STATUS_BAD_REQUEST, headers)
          .end('Request is missing search parameter `assetId`.')
        break
      }
      const parentDirectoryIdString = url.searchParams.get('parentDirectoryId')
      if (parentDirectoryIdString == null) {
        response
          .writeHead(HTTP_STATUS_BAD_REQUEST, headers)
          .end('Request is missing search parameter `parentDirectoryId`.')
        break
      }
      const parentDirectoryId = parentDirectoryIdString as DirectoryId
      const assetId = ProjectId(assetIdString)

      try {
        const defaultHeaders = await bodyJson<Record<string, string>>(request)
        const requestApiUrl = url.searchParams.get('apiUrl')
        if (requestApiUrl == null) {
          response
            .writeHead(HTTP_STATUS_BAD_REQUEST, headers)
            .end('Request is missing search parameter `apiUrl`.')
          break
        }
        const backend = createRemoteBackend(defaultHeaders, requestApiUrl)
        const fileName = 'project_root.enso-project'
        const uploadParams = {
          fileId: assetId,
          fileName,
          parentDirectoryId,
        }
        let uploadCount = 0
        const watcher = watch({
          directory: projectDir,
          delay: PROJECT_WATCHER_CALLBACK_DELAY,
          timeout: PROJECT_WATCHER_CALLBACK_TIMEOUT,
          callback: async () => {
            const responseBody = await projectManagement.createBundle(projectDir)
            const file = new File([responseBody.buffer as ArrayBuffer], fileName)
            // Overvwite uploads to create one version per session
            await uploadFile(backend, { ...uploadParams, overwrite: uploadCount > 0 }, file)
            uploadCount += 1
          },
        })

        const existingWatcher = watchers.get(assetId)
        if (existingWatcher) {
          await existingWatcher.close().catch((err) => {
            console.error(`Failed to stop project watcher ${assetId}`, err)
          })
        }
        watchers.set(assetId, watcher)
        response.writeHead(HTTP_STATUS_OK, headers).end()
      } catch (err) {
        console.error(`Failed to start project watcher ${assetId}`, err)
        response.writeHead(HTTP_STATUS_ERROR, headers).end()
      }
      break
    }
    case 'POST /api/watcher/stop': {
      const assetIdString = url.searchParams.get('assetId')
      if (assetIdString == null) {
        response
          .writeHead(HTTP_STATUS_BAD_REQUEST, headers)
          .end('Request is missing search parameter `assetId`.')
        break
      }
      const assetId = ProjectId(assetIdString)
      const watcher = watchers.get(assetId)
      if (watcher) {
        const isDirty = await watcher.close().catch((err) => {
          console.error(`Failed to stop project watcher ${assetId}`, err)
          return true
        })
        const status = isDirty ? HTTP_STATUS_IS_DIRTY : HTTP_STATUS_OK
        response.writeHead(status, headers).end()
      } else {
        response.writeHead(HTTP_STATUS_OK, headers).end()
      }
      break
    }
    case 'GET /api/watcher/state': {
      const assetIdString = url.searchParams.get('assetId')
      if (assetIdString == null) {
        response
          .writeHead(HTTP_STATUS_BAD_REQUEST, headers)
          .end('Request is missing search parameter `assetId`.')
        break
      }
      const assetId = ProjectId(assetIdString)
      const watcher = watchers.get(assetId)
      if (watcher) {
        const state = watcher.getState()
        const status = state === 'pending' ? HTTP_STATUS_IS_DIRTY : HTTP_STATUS_OK
        response.writeHead(status, headers).end()
      } else {
        response.writeHead(HTTP_STATUS_NOT_FOUND, headers).end()
      }
      break
    }
    default: {
      throw new Error(`Unknown watcher request ${requestPath}`)
    }
  }
}
