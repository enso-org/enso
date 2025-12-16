import { globSync } from 'node:fs'
import { extname, relative } from 'node:path'
import { fileURLToPath } from 'node:url'
import { defineConfig } from 'vite'
import dts from 'vite-plugin-dts'

// https://vitejs.dev/config/
export default defineConfig({
  mode: process.env.MODE,
  plugins: [dts()],
  build: {
    lib: {
      entry: Object.fromEntries(
        globSync('src/**/*.ts').map((path) => [
          relative('src', path.slice(0, path.length - extname(path).length)),
          path,
        ]),
      ),
      formats: ['es'],
    },
    minify: false,
  },
  cacheDir: fileURLToPath(new URL('../../node_modules/.cache/vite', import.meta.url)),
  envPrefix: 'ENSO_IDE_',
  logLevel: 'info',
})
