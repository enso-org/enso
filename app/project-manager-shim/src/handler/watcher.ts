import type * as http from 'node:http'
import { watch, type Watcher } from '../fs.js'
import { bodyJson } from './http.js'

const HTTP_STATUS_OK = 200
const HTTP_STATUS_NOT_FOUND = 404
const HTTP_STATUS_ERROR = 500

/** Map of directory paths to active watchers */
const watchers = new Map<string, Watcher>()

/** Check if this is a watcher request */
export function isWatcherRequest(requestPath: string): boolean {
  return requestPath.startsWith('/api/watcher/')
}

/** Handle watcher requests */
export async function handleWatcherRequest(
  request: http.IncomingMessage,
  response: http.ServerResponse,
  requestPath: string,
  headers: Record<string, string>,
  callback: () => Promise<void>,
): Promise<void> {
  switch (`${request.method} ${requestPath}`) {
    case 'POST /api/watcher/watch': {
      interface Body {
        readonly directory: string
        readonly delay: number
        readonly timeout: number
      }
      try {
        const { directory, delay, timeout } = await bodyJson<Body>(request)
        const existingWatcher = watchers.get(directory)
        if (existingWatcher) {
          await existingWatcher.close()
        }
        const watcher = watch({ directory, delay, timeout, callback })
        watchers.set(directory, watcher)
        response.writeHead(HTTP_STATUS_OK, headers).end()
      } catch (error) {
        response.writeHead(HTTP_STATUS_ERROR, headers).end(JSON.stringify({ error }))
      }
      break
    }
    case 'POST /api/watcher/close': {
      interface Body {
        readonly directory: string
      }
      try {
        const { directory } = await bodyJson<Body>(request)
        const watcher = watchers.get(directory)
        if (watcher) {
          const isUnsaved = await watcher.close()
          watchers.delete(directory)
          response.writeHead(HTTP_STATUS_OK, headers).end(JSON.stringify({ isUnsaved }))
        } else {
          response.writeHead(HTTP_STATUS_NOT_FOUND, headers).end()
        }
      } catch (error) {
        response.writeHead(HTTP_STATUS_ERROR, headers).end(JSON.stringify({ error }))
      }
      break
    }
    default: {
      throw new Error(`Unknown project service request ${requestPath}`)
    }
  }
}
