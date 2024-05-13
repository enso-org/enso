/***
 * This is a web entrypoint file for the GUI application running in e2e tests (playwright). It is
 * not included in normal application distribution. The goal of separate entrypoint is to allow
 * providing mocks for connections with engine and to avoid running dashboard.
 */

import { appRunner } from '@/appRunner'
import { MockYdocProvider } from '@/util/crdt'
import { MockTransport, MockWebSocket } from '@/util/net'
import { createPinia } from 'pinia'
import { mockDataHandler, mockLSHandler, mockYdocProvider } from '../mock/engine'

import 'enso-dashboard/src/tailwind.css'

MockTransport.addMock('engine', mockLSHandler)
MockWebSocket.addMock('data', mockDataHandler)
MockYdocProvider.addMock('engine', mockYdocProvider)

const window_ = window as any
// Mock FileBrowserApi that is usually provided by Electron.
window_.fileBrowserApi = {
  openFileBrowser: async () => {
    return ['/path/to/some/mock/file']
  },
}

const pinia = createPinia()
pinia.use((ctx) => {
  if (ctx.store.$id === 'graph') {
    window_.mockExpressionUpdate = ctx.store.mockExpressionUpdate
  }
})

// Instead of running through dashboard, setup the app immediately with mocked configuration.
appRunner.runApp(
  {
    startup: {
      project: 'Mock_Project',
      displayedProjectName: 'Mock Project',
    },
    engine: {
      rpcUrl: 'mock://engine',
      dataUrl: 'mock://data',
      namespace: 'local',
      projectManagerUrl: '',
    },
    window: {
      topBarOffset: '96',
    },
  },
  'project-135af445-bcfb-42fe-aa74-96f95e99c28b',
  null,
  undefined,
  pinia,
)
