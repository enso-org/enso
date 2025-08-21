import { sentryVitePlugin } from '@sentry/vite-plugin'
import react from '@vitejs/plugin-react'
import vue from '@vitejs/plugin-vue'
import * as fs from 'node:fs'
import { createWriteStream } from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import { pipeline } from 'node:stream/promises'
import { fileURLToPath } from 'node:url'
import postcssNesting from 'postcss-nesting'
import { findEnsoPath } from 'project-manager-shim'
import tailwindcss from 'tailwindcss'
import tailwindcssNesting from 'tailwindcss/nesting'
import { extract } from 'tar'
import { defaultClientConditions, defineConfig, type Plugin } from 'vite'
import VueDevTools from 'vite-plugin-vue-devtools'
import wasm from 'vite-plugin-wasm'
import tailwindConfig from './tailwind.config'

const isDevMode = process.env.NODE_ENV === 'development'
const isE2E = process.env.INTEGRATION_TEST === 'true'
const IS_ELECTRON_DEV_MODE = process.env.ELECTRON_DEV_MODE === 'true'

const entrypoint = isE2E ? './src/project-view/test-entrypoint.ts' : './src/entrypoint.ts'

if (isDevMode) {
  process.env.ENSO_IDE_YDOC_SERVER_URL ||= 'ws://__HOSTNAME__:5976'
}

if (isDevMode) {
  const projectRoot = fileURLToPath(new URL('../..', import.meta.url))
  let ensoExecutable = findEnsoPath(projectRoot)
  if (ensoExecutable === undefined) {
    await downloadEnsoEngine(projectRoot)
    ensoExecutable = findEnsoPath(projectRoot)
  }
  if (ensoExecutable) {
    console.log('Found enso executable:', ensoExecutable)
    process.env.ENSO_RUNNER_PATH = ensoExecutable
  }
}

// Used by vite middleware inside devtools plugin. Specifying this by an option doesn't work when `componentInspector` is false.
process.env.LAUNCH_EDITOR ??= 'code'

// https://vitejs.dev/config/
export default defineConfig({
  ...(IS_ELECTRON_DEV_MODE ? { root: fileURLToPath(new URL('.', import.meta.url)) } : {}),
  cacheDir: fileURLToPath(new URL('../../node_modules/.cache/vite', import.meta.url)),
  plugins: [
    wasm(),
    ...(isDevMode ?
      [
        await VueDevTools({
          // The JSX transform used by the inspector is causing react to complain and adds significant load time.
          componentInspector: false,
        }),
      ]
    : []),
    vue({
      customElement: ['**/components/visualizations/**', '**/components/shared/**'],
      template: {
        compilerOptions: {
          isCustomElement: (tag) => tag.startsWith('enso-'),
        },
      },
    }),
    react({
      include: [
        fileURLToPath(new URL('./src/**/*.tsx', import.meta.url)),
        fileURLToPath(new URL('./src/dashboard/**/use*.ts', import.meta.url)),
        fileURLToPath(new URL('./src/dashboard/**/*Hooks.ts', import.meta.url)),
      ],
    }),
    ...(process.env.DASHBOARD_TESTS !== 'true' ? [await projectManagerShim()] : []),
    ...((
      process.env.SENTRY_AUTH_TOKEN != null &&
      process.env.ENSO_IDE_SENTRY_ORGANIZATION != null &&
      process.env.ENSO_IDE_SENTRY_PROJECT != null
    ) ?
      [
        sentryVitePlugin({
          org: process.env.ENSO_IDE_SENTRY_ORGANIZATION,
          project: process.env.ENSO_IDE_SENTRY_PROJECT,
          ...(process.env.ENSO_IDE_VERSION != null ?
            { release: { name: process.env.ENSO_IDE_VERSION } }
          : {}),
        }),
      ]
    : []),
  ],
  optimizeDeps: {
    entries: fileURLToPath(new URL('./index.html', import.meta.url)),
    exclude: ['enso-common'],
    holdUntilCrawlEnd: true,
  },
  server: {
    warmup: {
      // Warming server up ***significantly*** speeds up execution of the first batch of tests in dev mode.
      clientFiles: [
        './src/**/*.vue',
        './src/**/*.tsx',
        './src/dashboard/hooks/**/*.ts',
        './src/dashboard/tailwind.css',
        './node_modules/@tanstack/**/*.js',
      ],
    },
    headers: {
      'Cross-Origin-Opener-Policy': 'same-origin',
      'Cross-Origin-Resource-Policy': 'same-origin',
    },
    ...(process.env.GUI_HOSTNAME ? { host: process.env.GUI_HOSTNAME } : {}),
  },
  resolve: {
    conditions: isDevMode ? ['source', ...defaultClientConditions] : [...defaultClientConditions],
    alias: {
      '/src/entrypoint.ts': fileURLToPath(new URL(entrypoint, import.meta.url)),
      shared: fileURLToPath(new URL('./shared', import.meta.url)),
      '@': fileURLToPath(new URL('./src/project-view', import.meta.url)),
      '#': fileURLToPath(new URL('./src/dashboard', import.meta.url)),
      $: fileURLToPath(new URL('./src', import.meta.url)),
    },
  },
  envPrefix: 'ENSO_IDE_',
  define: {
    // Single hardcoded usage of `global` in aws-amplify.
    'global.TYPED_ARRAY_SUPPORT': true,
  },
  esbuild: {
    dropLabels: isDevMode ? [] : ['DEV'],
    supported: {
      'top-level-await': true,
    },
  },
  assetsInclude: ['**/*.svg'],
  css: {
    postcss: {
      plugins: [tailwindcssNesting(postcssNesting()), tailwindcss(tailwindConfig)],
    },
  },
  logLevel: 'info',
  build: {
    // dashboard chunk size is larger than the default warning limit
    chunkSizeWarningLimit: 700,
    sourcemap: true,
    rollupOptions: {
      output: {
        manualChunks: {
          config: ['./src/config'],
          entrypoint: ['./src/entrypoint'],
        },
      },
    },
  },
  preview: {
    port: 5173,
  },
})

async function projectManagerShim(): Promise<Plugin> {
  const module = await import('./project-manager-shim-middleware')
  return {
    name: 'project-manager-shim',
    configureServer(server) {
      server.middlewares.use(module.default)
    },
    configurePreviewServer(server) {
      server.middlewares.use(module.default)
    },
  }
}

async function downloadEnsoEngine(projectRoot: string): Promise<string> {
  console.log('Downloading latest Enso engine...')

  // Determine platform and architecture
  const platform = os.platform()
  const arch = os.arch()

  let platformString: string
  if (platform === 'darwin') {
    platformString = 'macos'
  } else if (platform === 'linux') {
    platformString = 'linux'
  } else if (platform === 'win32') {
    platformString = 'windows'
  } else {
    throw new Error(`Unsupported platform: ${platform}`)
  }

  let archString: string
  if (arch === 'x64') {
    archString = 'amd64'
  } else if (arch === 'arm64') {
    archString = 'aarch64'
  } else {
    throw new Error(`Unsupported architecture: ${arch}`)
  }

  // Fetch all releases from GitHub API and find the latest prerelease
  const releasesUrl = 'https://api.github.com/repos/enso-org/enso/releases'
  const releasesResponse = await fetch(releasesUrl)

  if (!releasesResponse.ok) {
    throw new Error(`Failed to fetch releases: ${releasesResponse.statusText}`)
  }

  const releases = await releasesResponse.json()

  // Find the latest prerelease
  const latestPrerelease = releases.find((release: any) => release.prerelease)

  if (!latestPrerelease) {
    throw new Error('No prerelease found')
  }

  const releaseData = latestPrerelease
  const version = releaseData.tag_name

  // Find the matching asset
  const assetName = `enso-engine-${version}-${platformString}-${archString}.tar.gz`
  const asset = releaseData.assets.find((a: any) => a.name === assetName)

  if (!asset) {
    throw new Error(`Could not find asset: ${assetName}`)
  }

  console.log(`Downloading ${assetName}...`)

  // Download the asset
  const downloadResponse = await fetch(asset.browser_download_url)

  if (!downloadResponse.ok) {
    throw new Error(`Failed to download asset: ${downloadResponse.statusText}`)
  }

  // Create the built-distribution directory if it doesn't exist
  const distDir = path.join(projectRoot, 'built-distribution')
  if (!fs.existsSync(distDir)) {
    fs.mkdirSync(distDir, { recursive: true })
  }

  // Save and extract the archive
  const archivePath = path.join(distDir, assetName)
  const extractDir = path.join(distDir, assetName.replace('.tar.gz', ''))

  // Create extract directory if it doesn't exist
  if (!fs.existsSync(extractDir)) {
    fs.mkdirSync(extractDir, { recursive: true })
  }

  // Download and save the file
  const fileStream = createWriteStream(archivePath)
  await pipeline(downloadResponse.body as any, fileStream)

  console.log(`Extracting to ${extractDir}...`)

  // Extract the archive
  await pipeline(
    fs.createReadStream(archivePath),
    extract({
      cwd: extractDir,
    }),
  )

  // Clean up the archive file
  fs.unlinkSync(archivePath)

  console.log(`Enso engine downloaded and extracted to ${extractDir}`)

  return extractDir
}
