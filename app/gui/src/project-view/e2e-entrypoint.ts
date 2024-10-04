/***
 * This is a web entrypoint file for the GUI application running in e2e tests (playwright). It is
 * not included in normal application distribution. The goal of separate entrypoint is to allow
 * providing mocks for connections with engine and to avoid running dashboard.
 */

import { MockYdocProvider } from '@/util/crdt'
import { MockWebSocket, MockWebSocketTransport } from '@/util/net'
import { mockDataHandler, mockLSHandler, mockYdocProvider } from './mock/engine'

import '#/tailwind.css'
import { createApp } from 'vue'
import { AsyncApp } from './asyncApp'

MockWebSocketTransport.addMock('engine', mockLSHandler)
MockWebSocket.addMock('data', mockDataHandler)
MockYdocProvider.addMock('engine', mockYdocProvider)

const window_ = window as any
// Mock FileBrowserApi that is usually provided by Electron.
window_.fileBrowserApi = {
  openFileBrowser: async () => {
    return ['/path/to/some/mock/file']
  },
}

AsyncApp().then(({ default: App }) => {
  const app = createApp(App, {
    config: {
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
    projectId: 'project-135af445-bcfb-42fe-aa74-96f95e99c28b',
    logEvent: () => {},
    hidden: false,
  })
  app.mount('body')
})
