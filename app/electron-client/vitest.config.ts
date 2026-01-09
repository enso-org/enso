import { fileURLToPath } from 'node:url'
import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    // Increase the default test timeout to accommodate longer-running tests.
    testTimeout: 120_000,
    include: ['tests/headless/**/*.test.ts'],
    silent: 'passed-only',
  },
  resolve: {
    alias: {
      '@': fileURLToPath(new URL('./src', import.meta.url)),
    },
  },
})
