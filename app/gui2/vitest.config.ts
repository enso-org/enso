import { loadTestEnvironmentVariables } from 'enso-common/src/appConfig'
import { fileURLToPath } from 'node:url'
import { configDefaults, defineConfig, mergeConfig } from 'vitest/config'

loadTestEnvironmentVariables()
const viteConfig = (await import('./vite.config')).default

export default mergeConfig(
  viteConfig,
  defineConfig({
    test: {
      environment: 'jsdom',
      includeSource: ['./{src,shared,ydoc-server}/**/*.{ts,vue}'],
      exclude: [...configDefaults.exclude, 'e2e/*'],
      root: fileURLToPath(new URL('./', import.meta.url)),
      restoreMocks: true,
    },
    define: {
      RUNNING_VITEST: true,
    },
  }),
)
