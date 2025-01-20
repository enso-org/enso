import { globalExternals } from '@fal-works/esbuild-plugin-global-externals'
import esbuild from 'esbuild'
import fs from 'fs/promises'
import path from 'path'
import url from 'url'

const watchMode = process.argv[2] === 'watch'
const globals = {
  'node:zlib': {
    varName: 'zlib',
    type: 'cjs',
  },
}

const ctx = await esbuild.context({
  outfile: 'dist/main.cjs',
  sourcemap: 'linked',
  entryPoints: ['src/main.ts'],
  bundle: true,
  platform: 'browser',
  define: {
    self: 'globalThis',
  },
  plugins: [usePolyglotFfi(), globalExternals(globals)],
  conditions: watchMode ? ['source'] : [],
  external: ['node:url'], // Not actually used, tree-shaken out
  format: 'cjs',
})
if (watchMode) await ctx.watch()
else {
  const result = await ctx.rebuild()
  await ctx.dispose()
}

/** @type () => esbuild.Plugin */
function usePolyglotFfi() {
  return {
    name: 'use-polyglot-ffi',
    setup(build) {
      const newPath = url.fileURLToPath(new URL('./src/ffiPolyglot.ts', import.meta.url))
      build.onLoad(
        {
          filter: /ydoc-shared.*ast(\\|\/)ffi.(js|ts)$/,
        },
        async () => {
          return {
            contents: await fs.readFile(newPath),
            resolveDir: path.dirname(newPath),
            loader: 'ts',
          }
        },
      )
    },
  }
}
