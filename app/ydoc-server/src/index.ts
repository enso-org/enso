/**
 * @file An entry point for the Yjs gateway server. The gateway server is a WebSocket server that
 * synchronizes document requests and updates between language server and clients connected to the
 * Yjs document mesh. It also serves as a central point for synchronizing document data and
 * awareness updates between clients.
 *
 * Currently, this server is being run automatically in background as part of the vite development
 * server. It is not yet deployed to any other environment.
 */

import debug from 'debug'
import { docName } from './auth'
import { deserializeIdMap } from './serialization'
import { setupGatewayClient, WSSharedDoc, YjsConnection, type YjsSocket } from './ydoc'

export { InspectManager } from './inspect.js'
export { deserializeIdMap, docName, setupGatewayClient, WSSharedDoc, YjsConnection, type YjsSocket }

/** @param customLogger Optional external logger to use for all debug logs. */
export function configureAllDebugLogs(
  forceEnable: boolean,
  customLogger?: (...args: any[]) => any,
): void {
  for (const debugModule of ['ydoc-server:session', 'ydoc-shared:languageServer']) {
    const instance = debug(debugModule)
    if (forceEnable) instance.enabled = true
    if (customLogger) instance.log = customLogger
  }
}
