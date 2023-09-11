import { fileURLToPath } from 'node:url'
import path from 'node:path'
import { defineConfig, Plugin } from 'vite'
import vue from '@vitejs/plugin-vue'
import postcssNesting from 'postcss-nesting'
import { createGatewayServer } from './server'
import * as esbuild from 'esbuild'

const projectManagerUrl = 'ws://127.0.0.1:30535'
import * as dashboardBundler from '../ide-desktop/lib/dashboard/esbuild-config'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [vue(), gatewayServer(), dashboardBundle()],
  resolve: {
    alias: {
      shared: fileURLToPath(new URL('./shared', import.meta.url)),
      '@': fileURLToPath(new URL('./src', import.meta.url)),
    },
  },
  define: {
    REDIRECT_OVERRIDE: JSON.stringify('http://localhost:8080'),
    PROJECT_MANAGER_URL: JSON.stringify(projectManagerUrl),
    global: 'window',
    IS_DEV_MODE: JSON.stringify(process.env.NODE_ENV !== 'production'),
  },
  assetsInclude: ['**/*.yaml'],
  css: {
    postcss: {
      plugins: [postcssNesting],
    },
  },
})

function gatewayServer(): Plugin {
  return {
    name: 'y-websocket-server',
    configureServer(server) {
      if (server.httpServer == null) return

      createGatewayServer(server.httpServer)
    },
  }
}

async function dashboardBundle(): Promise<Plugin> {
  const outputPath = path.resolve('../ide-desktop/lib/dashboard/src')

  const options = dashboardBundler.bundlerOptions({
    devMode: process.env.NODE_ENV !== 'production',
    outputPath,
  })
  options.write = false
  options.metafile = true
  options.sourcemap = options.minify ? false : 'inline'

  const builderPromise = esbuild.context(options)
  let matchingIds: string[] = []
  let commonPrefix = ''
  async function doRebuild() {
    const builder = await builderPromise
    const result = await builder.rebuild()
    matchingIds = result.outputFiles?.map((f) => f.path.replaceAll('\\', '/')) ?? []
    commonPrefix = matchingIds?.reduce((a, b) => {
      let i = 0
      while (a[i] === b[i]) i++
      return a.slice(0, i)
    })
    return result
  }

  await doRebuild()

  return {
    name: 'enso-dashboard-bundle',
    async load(id) {
      if (id.startsWith(commonPrefix) && matchingIds?.includes(id)) {
        const build = await doRebuild()
        if (build.errors?.length) {
          for (const error of build.errors) {
            this.error({
              id: error.id,
              message: error.text,
              loc: error.location ?? undefined,
            })
          }
        }
        const matchedFile = build.outputFiles?.find((f) => id === f.path.replaceAll('\\', '/'))
        if (matchedFile) {
          const metaKey = path
            .relative(options.absWorkingDir, matchedFile.path)
            .replaceAll('\\', '/')
          const outputMeta = build.metafile?.outputs[metaKey]
          if (outputMeta) {
            for (const inputFile of Object.keys(outputMeta?.inputs ?? {})) {
              if (inputFile.startsWith('src')) {
                const fullPath = path.resolve(options.absWorkingDir, inputFile)
                this.addWatchFile(fullPath)
              }
            }
          }
          return matchedFile.text
        }
        this.error(
          `Could not find '${id}' in dashboard output files. ` +
            `Consider adding it to the daskboard esbuild entrypoints.`,
        )
      }
    },
  }
}
