// Shared node-mode Vitest config: inline workspace deps and honor the custom
// `source` export condition so tests use updated sources instead of stale dist.
const nodeVitestConfig = {
  ssr: {
    resolve: {
      conditions: ['source', 'import', 'default'],
    },
  },
  test: {
    server: {
      deps: {
        inline: [/^enso-/, /^ydoc-/],
      },
    },
  },
}

export default nodeVitestConfig
