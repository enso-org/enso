import { fileURLToPath } from 'node:url'
import { loadEnv } from 'vite'
import { configDefaults, defineConfig, mergeConfig } from 'vitest/config'
import viteConfig from './vite.config'

const config = mergeConfig(
  viteConfig,
  defineConfig({
    test: {
      // We need to load environment variables locally: https://github.com/vitest-dev/vitest/issues/1148
      // Supposedly, vitest only loads environment variables beginning with VITE_.
      // We use ENSO_IDE_ prefix for our variables, so we have to load them manually.
      env: loadEnv('testing', '../gui', 'ENSO_IDE_'),
      reporters: process.env.CI ? ['dot', 'github-actions'] : ['default'],
      environment: 'jsdom',
      includeSource: ['./src/**/*.{ts,tsx,vue}'],
      exclude: [...configDefaults.exclude, 'integration-test/**/*'],
      root: fileURLToPath(new URL('./', import.meta.url)),
      restoreMocks: true,
      setupFiles: './src/dashboard/test/setup.ts',
    },
  }),
)
config.esbuild.dropLabels = config.esbuild.dropLabels.filter((label: string) => label != 'DEV')
config.resolve.conditions = config.resolve.conditions.filter((c: string) => c != 'module') // work around vitest bug that forces esm imports in commonjs deps
export default config
