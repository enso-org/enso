import { HstVue } from '@histoire/plugin-vue'
import { defineConfig } from 'histoire'

export default defineConfig({
  setupFile: './stories/setup.ts',
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
    server: {
      fs: {
        allow: ['../..'],
      },
    },
  },
})
