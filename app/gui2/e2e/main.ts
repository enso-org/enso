import 'enso-dashboard/src/tailwind.css'
import { createPinia } from 'pinia'
import { createApp, ref } from 'vue'
import { mockDataHandler, mockLSHandler } from '../mock/engine'
import '../src/assets/base.css'
import { provideGuiConfig } from '../src/providers/guiConfig'
import { provideVisualizationConfig } from '../src/providers/visualizationConfig'
import { Vec2 } from '../src/util/data/vec2'
import { MockTransport, MockWebSocket } from '../src/util/net'
import MockApp from './MockApp.vue'

MockTransport.addMock('engine', mockLSHandler)
MockWebSocket.addMock('data', mockDataHandler)

const app = createApp(MockApp)
app.use(createPinia())
provideGuiConfig._mock(
  ref({
    startup: {
      project: 'Mock',
      displayedProjectName: 'Mock Project',
    },
    engine: { rpcUrl: 'mock://engine', dataUrl: 'mock://data' },
  }),
  app,
)
// Required for visualization stories.
provideVisualizationConfig._mock(
  {
    fullscreen: false,
    width: 200,
    height: 150,
    hide() {},
    isCircularMenuVisible: false,
    nodeSize: new Vec2(200, 150),
    currentType: {
      module: { kind: 'Builtin' },
      name: 'Current Type',
    },
    types: [
      {
        module: { kind: 'Builtin' },
        name: 'Example',
      },
      {
        module: { kind: 'Builtin' },
        name: 'Types',
      },
      {
        module: { kind: 'Builtin' },
        name: 'Here',
      },
    ],
    updateType() {},
  },
  app,
)
app.mount('#app')
