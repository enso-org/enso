import { onScopeDispose } from 'vue'
import { YjsChannel } from 'ydoc-channel'
import { AbortScope } from 'ydoc-shared/util/net'
import { YjsTransport } from 'ydoc-shared/util/net/YjsTransport'
import * as Y from 'yjs'

export { AsyncQueue } from 'enso-common/src/utilities/async'
export { AbortScope }

/** TODO: Add docs */
export function createRpcTransport(indexDoc: Y.Doc, url: string): YjsTransport {
  return new YjsTransport(indexDoc, url)
}

/** TODO: Add docs */
export function createDataSocket(indexDoc: Y.Doc, url: string): YjsChannel {
  return new YjsChannel(indexDoc, url)
}

export interface WebSocketHandler {
  (
    data: string | ArrayBufferLike | Blob | ArrayBufferView,
    send: (data: string | ArrayBufferLike | Blob | ArrayBufferView) => void,
  ): void
}

/** Create an abort signal that is signalled when containing Vue scope is disposed. */
export function useAbortScope(): AbortScope {
  const scope = new AbortScope()
  onScopeDispose(() => scope.dispose('Vue scope disposed.'))
  return scope
}
