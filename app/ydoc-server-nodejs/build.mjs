import esbuild from 'esbuild'
import { wasmLoader } from 'esbuild-plugin-wasm'

const watchMode = process.argv[2] === 'watch'

const ctx = await esbuild.context({
  outfile: 'dist/main.mjs',
  sourcemap: 'linked',
  entryPoints: ['src/main.ts'],
  bundle: true,
  platform: 'node',
  target: ['node20'],
  conditions: watchMode ? ['source'] : [],
  format: 'esm',
  plugins: [wasmLoader()],
})
if (watchMode) await ctx.watch()
else {
  await ctx.rebuild()
  await ctx.dispose()
}
