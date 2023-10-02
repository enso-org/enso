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
  vite: {
    resolve: {
      alias: {
        'https://cdn.jsdelivr.net/npm/d3@7.8.5/+esm': require.resolve('d3'),
      },
    },
    define: {
      HISTOIRE: 'true',
    },
  },
})
