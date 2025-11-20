import { AssetId, DirectoryId, ProjectId, type GetText } from 'enso-common/src/services/Backend'
import { HttpClient } from 'enso-common/src/services/HttpClient'
import { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import { getText, resolveDictionary } from 'enso-common/src/text'
import type * as http from 'node:http'
import { watch, type Watcher, type WatchOptions } from '../fs.js'
import * as projectManagement from '../projectManagement.js'
import { uploadFile } from '../upload.js'
import { bodyJson } from './http.js'

const HTTP_STATUS_OK = 200
const HTTP_STATUS_BAD_REQUEST = 400
const HTTP_STATUS_ERROR = 500

/** Check if this is a watcher request */
export function isWatcherRequest(requestPath: string): boolean {
  return requestPath.startsWith('/api/watcher/')
}

/** Handle watcher requests. */
export async function handleWatcherRequest(
  request: http.IncomingMessage,
  response: http.ServerResponse,
  headers: Record<string, string>,
  watchers: Map<AssetId, Watcher>,
  options: Pick<WatchOptions, 'delay' | 'timeout'>,
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
      const baseUrl = url.searchParams.get('baseUrl')
      if (baseUrl == null) {
        response
          .writeHead(HTTP_STATUS_BAD_REQUEST, headers)
          .end('Request is missing search parameter `baseUrl`.')
        break
      }
      const assetId = ProjectId(assetIdString)

      try {
        const defaultHeaders = await bodyJson<Record<string, string>>(request)
        const client = new HttpClient(defaultHeaders)
        const downloader = () => {}
        const dictionary = resolveDictionary()
        const backendGetText: GetText = function (key, ...replacements) {
          return getText(dictionary, key, ...replacements)
        }
        const backend = new RemoteBackend(backendGetText, client, downloader, new URL(baseUrl))
        const fileName = 'project_root.enso-project'
        const uploadParams = {
          fileId: assetId,
          fileName,
          parentDirectoryId: parentDirectoryIdString as DirectoryId,
        }
        const watcher = watch({
          directory: projectDir,
          delay: options.delay,
          timeout: options.timeout,
          callback: async () => {
            const responseBody = await projectManagement.createBundle(projectDir)
            const file = new File([responseBody.buffer as ArrayBuffer], fileName)
            await uploadFile(backend, uploadParams, file)
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
        await watcher
          .close()
          .catch((err) => {
            console.error(`Failed to stop project watcher ${assetId}`, err)
          })
          .finally(() => {
            response.writeHead(HTTP_STATUS_OK, headers).end()
          })
      } else {
        response.writeHead(HTTP_STATUS_OK, headers).end()
      }
      break
    }
    default: {
      throw new Error(`Unknown watcher request ${requestPath}`)
    }
  }
}
