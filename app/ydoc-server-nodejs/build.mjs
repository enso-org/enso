import esbuild from 'esbuild'
import { wasmLoader } from 'esbuild-plugin-wasm'

const ctx = await esbuild.context({
  outfile: 'dist/main.mjs',
  sourcemap: 'linked',
  entryPoints: ['src/main.ts'],
  bundle: true,
  platform: 'node',
  target: ['node20'],
  format: 'esm',
  plugins: [wasmLoader()],
})
if (process.argv[2] === 'watch') await ctx.watch()
else {
  await ctx.rebuild()
  await ctx.dispose()
}
