import { Server } from '@open-rpc/server-js'
import * as random from 'lib0/random'
import pmSpec from './pm-openrpc.json' assert { type: 'json' }
import {
  methods as pmMethods,
  projects,
  type ProjectId,
  type ProjectName,
  type UTCDateTime,
} from './projectManager'

/**
 * Setup for all E2E tests.
 *
 * It runs mocked project manager server.
 */
export default function setup() {
  const pm = new Server({
    transportConfigs: [
      {
        type: 'WebSocketTransport',
        options: {
          id: 'websocket',
          udp: true,
          ipv6: true,
          port: 30536,
          middleware: [],
        },
      },
    ],
    openrpcDocument: pmSpec as typeof pmSpec & { openrpc: never },
    methodMapping: pmMethods,
  })
  pm.start()
  projects.set('mock project id 0001', {
    id: random.uuidv4() as ProjectId,
    created: new Date().toISOString() as UTCDateTime,
    lastOpened: new Date().toISOString() as UTCDateTime,
    engineVersion: '',
    name: 'Mock Project Name' as ProjectName,
    namespace: 'local',
  })
}
