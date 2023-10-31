import { HstVue } from '@histoire/plugin-vue'
import { defineConfig } from 'histoire'

const order = [
  // Graph
  'Editor',
  'Widgets',
  'Code Editor',
  'Component Browser',
  'Node',
  'Top Bar',
  'Circular Menu',
  'Selection Brush',
  // Miscellaneous
  'SVG Icon',
  'All SVG Icons',
  // Visualizations
  'Selector',
  'JSON',
  'Table',
  'Scatterplot',
  'Histogram',
  'Heatmap',
  'SQL Query',
  'Geo Map',
  'Image',
  'Warnings',
]

export default defineConfig({
  theme: {
    title: 'Enso Demo Scenes',
  },
  setupFile: './stories/setup.ts',
  plugins: [HstVue()],
  tree: {
    groups: [
      { id: 'graph', title: 'Graph' },
      { id: 'misc', title: 'Miscellaneous' },
      { id: 'visualizations', title: 'Visualizations' },
    ],
    order(a, b) {
      const aIndex = order.indexOf(a)
      const bIndex = order.indexOf(b)
      return aIndex != null
        ? bIndex != null
          ? aIndex - bIndex
          : -1
        : bIndex != null
        ? 1
        : a.localeCompare(b)
    },
  },
  vite: {
    server: {
      fs: {
        allow: ['../..'],
      },
    },
  },
})
