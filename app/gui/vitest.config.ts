import { fileURLToPath } from 'node:url'
import { configDefaults, defineConfig, mergeConfig } from 'vitest/config'
import viteConfig from './vite.config'

const config = mergeConfig(
  viteConfig,
  defineConfig({
    test: {
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
