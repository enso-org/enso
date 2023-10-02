import { ref } from 'vue'

import { defineSetupVue3 } from '@histoire/plugin-vue'
import { createPinia } from 'pinia'

import { guiConfigProvideKey$FOR$INTERNAL$USE$ONLY } from '@/providers/guiConfig'
import { visualizationConfigProvideKey$FOR$INTERNAL$USE$ONLY } from '@/providers/visualizationConfig'

import '@/assets/base.css'
import { Vec2 } from '@/util/vec2'

export const setupVue3 = defineSetupVue3(({ app }) => {
  // Required for graph stories.
  app.use(createPinia())
  app.provide(
    guiConfigProvideKey$FOR$INTERNAL$USE$ONLY,
    ref({
      startup: {
        project: 'Mock Project',
      },
      engine: { rpcUrl: 'ws://[100::]', dataUrl: 'ws://[100::]' },
    }),
  )
  // Required for visualization stories.
  app.provide(
    visualizationConfigProvideKey$FOR$INTERNAL$USE$ONLY,
    ref({
      fullscreen: false,
      width: 200,
      height: 150,
      hide() {},
      isCircularMenuVisible: false,
      nodeSize: new Vec2(200, 150),
      types: ['Example', 'Types', 'Here'],
      updateType() {},
    }),
  )
})
