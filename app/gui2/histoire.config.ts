import { HstVue } from '@histoire/plugin-vue'
import { defineConfig } from 'histoire'

// For some reason `import '@/assets/base.css'` in a `setupFile` does not work.

export default defineConfig({
  plugins: [HstVue()],
  tree: {
    groups: [
      { id: 'graph', title: 'Graph' },
      { id: 'widgets', title: 'Widgets' },
      { id: 'visualizations', title: 'Visualizations' },
    ],
  },
})
