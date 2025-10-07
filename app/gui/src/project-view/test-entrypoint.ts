/***
 * This is a web entrypoint file for the GUI application running in integration tests (playwright). It is
 * not included in normal application distribution. The goal of separate entrypoint is to allow
 * providing mocks for connections with engine and to avoid running dashboard.
 */

import { MockYdocProvider } from '@/util/crdt'
import { MockWebSocket, MockWebSocketTransport } from '@/util/net'
import { mockDataHandler, mockLSHandler, mockYdocProvider } from './mock/engine'

import '#/styles.css'
import '#/tailwind.css'
import router from '$/router'
import '@/assets/base.css'
import { VueQueryPlugin } from '@tanstack/vue-query'
import { createApp } from 'vue'
import App from '../App.vue'

MockWebSocketTransport.addMock('engine', mockLSHandler)
MockWebSocket.addMock('data', mockDataHandler)
MockYdocProvider.addMock('engine', mockYdocProvider)

const window_ = window as any
// Mock FileBrowserApi that is usually provided by Electron.
window_.api = {
  fileBrowser: {
    openFileBrowser: async () => {
      return ['/path/to/some/mock/file']
    },
  },
}

const app = createApp(App, {
  projectViewOnly: {
    options: {
      projectId: 'project-135af445-bcfb-42fe-aa74-96f95e99c28b',
      projectInitialName: 'Mock_Project',
      projectDisplayedName: 'Mock Project',
      projectPath: 'enso://User/mock/Mock Project',
      projectNamespace: 'local',
      engine: {
        rpcUrl: 'mock://engine',
        dataUrl: 'mock://data',
      },
      hidden: false,
      logEvent: () => {},
      renameProject: () => {
        throw new Error('Renaming project not supported in test environment.')
      },
    },
  },
  rootDirPath: '',
})
app.use(VueQueryPlugin)
app.use(router)
app.mount('body')
