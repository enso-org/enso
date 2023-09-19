import { Client } from '@open-rpc/client-js'

import * as map from 'lib0/map'
import * as set from 'lib0/set'
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

  addEventListener<K extends keyof Notifications>(
    type: K,
    listener: (params: Notifications[K]) => void,
  ) {
    const listeners = map.setIfUndefined(this.handlers, type as string, set.create)
    listeners.add(listener)
  }

  removeEventListener<K extends keyof Notifications>(
    type: K,
    listener: (params: Notifications[K]) => void,
  ) {
    const listeners = this.handlers.get(type as string)
    if (listeners) {
      listeners.delete(listener)
    }
  }

  private request(method: string, params: object): Promise<any> {
    return this.client.request({ method, params })
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
