import '@/assets/base.css'
import { provideGuiConfig } from '@/providers/guiConfig'
import { provideVisualizationConfig } from '@/providers/visualizationConfig'
import { Vec2 } from '@/util/vec2'
import { defineSetupVue3 } from '@histoire/plugin-vue'
import { createPinia } from 'pinia'
import { ref } from 'vue'
import './story.css'

export const setupVue3 = defineSetupVue3(({ app }) => {
  // Required for graph stories.
  app.use(createPinia())
  app.runWithContext(() =>
    provideGuiConfig(
      ref({
        startup: {
          project: 'Mock Project',
          displayedProjectName: 'Mock Project',
        },
        engine: { rpcUrl: 'ws://[100::]', dataUrl: 'ws://[100::]' },
      }),
    ),
  )
  // Required for visualization stories.
  app.runWithContext(() => {
    provideVisualizationConfig({
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
    })
  })
})
