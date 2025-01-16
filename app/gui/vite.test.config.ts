/** @file Vite configuration for dashboard integration tests' server. */
import { fileURLToPath } from 'node:url'

import { defineConfig, mergeConfig } from 'vite'

// =====================
// === Configuration ===
// =====================

// This configuration file is for dashboard tests only.
process.env.CLOUD_BUILD = 'true'
const CONFIG = (await import('./vite.config')).default

export default mergeConfig(
  CONFIG,
  defineConfig({
    mode: 'testing', // load environment from .env.testing file
    plugins: [
      {
        name: 'load-svg',
        enforce: 'pre',
        transform(_, id) {
          // Mock out SVGs that are used in the dashboard.
          if (id.endsWith('.svg')) {
            const svgContent = `<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 100 100">
  <defs>
    <pattern id="checkerboard" width="20" height="20" patternUnits="userSpaceOnUse">
      <rect width="10" height="10" fill="white"/>
      <rect x="10" y="0" width="10" height="10" fill="black"/>
      <rect x="0" y="10" width="10" height="10" fill="black"/>
      <rect x="10" y="10" width="10" height="10" fill="white"/>
    </pattern>
  </defs>
  <rect width="100" height="100" fill="url(#checkerboard)"/>
</svg>`
            const encodedSvg = `data:image/svg+xml,${encodeURIComponent(svgContent)}`
            return `export default \`${encodedSvg}\``
          }
        },
      },
    ],
    resolve: {
      alias: {
        '@stripe/stripe-js/pure': fileURLToPath(
          new URL('./integration-test/dashboard/mock/stripe.ts', import.meta.url),
        ),
        '@stripe/react-stripe-js': fileURLToPath(
          new URL('./integration-test/dashboard/mock/react-stripe.tsx', import.meta.url),
        ),
      },
      extensions: [
        '.mock.mjs',
        '.mock.js',
        '.mock.mts',
        '.mock.ts',
        '.mock.jsx',
        '.mock.tsx',
        '.mock.json',
        '.mjs',
        '.js',
        '.mts',
        '.ts',
        '.jsx',
        '.tsx',
        '.json',
      ],
    },
    build: {
      outDir: fileURLToPath(new URL('./mockDist', import.meta.url)),
    },
  }),
)
