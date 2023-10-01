import { Client } from '@open-rpc/client-js'

import { uuidv4 } from 'lib0/random'
import { SHA3 } from 'sha3'
import { Emitter } from './event'
import type {
  Checksum,
  FileEdit,
  Notifications,
  Path,
  RegisterOptions,
  response,
} from './languageServerTypes'
import type { Uuid } from './yjsModel'

const DEBUG_LOG_RPC = false

class RpcError extends Error {
  cause: Error
  request: string
  params: object
  constructor(inner: Error, request: string, params: object) {
    super(`Language server request failed.`)
    this.cause = inner
    this.request = request
    this.params = params
  }
}

export class LanguageServer extends Emitter<Notifications> {
  client: Client
  handlers: Map<string, Set<(...params: any[]) => void>>

  constructor(client: Client) {
    super()
    this.client = client
    this.handlers = new Map()

    client.onNotification((notification) => {
      this.emit(notification.method as keyof Notifications, [notification.params])
    })
  }
  private async request(method: string, params: object): Promise<any> {
    const uuid = uuidv4()
    const now = performance.now()
    try {
      if (DEBUG_LOG_RPC) {
        console.log(`LS [${uuid}] ${method}:`)
        console.dir(params)
      }
      return await this.client.request({ method, params }, 10000)
    } catch (e) {
      if (e instanceof Error) {
        throw new RpcError(e, method, params)
      }
      throw e
    } finally {
      if (DEBUG_LOG_RPC) {
        console.log(`LS [${uuid}] ${method} took ${performance.now() - now}ms`)
      }
    }
  }

  acquireCapability(method: string, registerOptions: RegisterOptions): Promise<void> {
    return this.request('capability/acquire', { method, registerOptions })
  }

  acquireReceivesTreeUpdates(path: Path): Promise<void> {
    return this.acquireCapability('file/receivesTreeUpdates', { path })
  }

  initProtocolConnection(clientId: Uuid): Promise<response.InitProtocolConnection> {
    return this.request('session/initProtocolConnection', { clientId })
  }

  openTextFile(path: Path): Promise<response.OpenTextFile> {
    return this.request('text/openFile', { path })
  }

  closeTextFile(path: Path): Promise<void> {
    return this.request('text/closeFile', { path })
  }

  saveTextFile(path: Path, currentVersion: Checksum): Promise<void> {
    return this.request('text/save', { path, currentVersion })
  }

  applyEdit(edit: FileEdit, execute: boolean): Promise<void> {
    return this.request('text/applyEdit', { edit, execute })
  }

  listFiles(path: Path): Promise<response.FileList> {
    return this.request('file/list', { path })
  }

  dispose() {
    this.client.close()
  }
}

export function computeTextChecksum(text: string): Checksum {
  const hash = new SHA3(224)
  hash.update(text)
  return hash.digest('hex') as Checksum
}
