import esbuild from 'esbuild'
import fs from 'fs/promises'
import path from 'path'
import url from 'url'

const ctx = await esbuild.context({
  outfile: 'dist/main.mjs',
  sourcemap: 'linked',
  entryPoints: ['src/main.ts'],
  bundle: true,
  platform: 'node',
  format: 'esm',
  conditions: ['source'],
  plugins: [useFfiStub()],
  banner: {
    js: "import { createRequire } from 'module'; const require = createRequire(import.meta.url);",
  },
})
await ctx.rebuild()
await ctx.dispose()

/** Replace ydoc-shared's FFI module with a local stub that doesn't need rust-ffi or hash-wasm. */
function useFfiStub() {
  return {
    name: 'use-ffi-stub',
    setup(build) {
      const stubPath = url.fileURLToPath(new URL('./src/ffiStub.ts', import.meta.url))
      build.onLoad(
        {
          filter: /ydoc-shared.*ast[\\/]ffi\.(js|ts)$/,
        },
        async () => {
          return {
            contents: await fs.readFile(stubPath),
            resolveDir: path.dirname(stubPath),
            loader: 'ts',
          }
        },
      )
    },
  }
}
