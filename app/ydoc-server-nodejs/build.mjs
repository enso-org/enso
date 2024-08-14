import esbuild from 'esbuild'
import { wasmLoader } from 'esbuild-plugin-wasm'
import { fork } from 'node:child_process'

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
  plugins: [wasmLoader(), ...(watchMode ? [runServerProcess()] : [])],
})
if (watchMode) await ctx.watch()
else {
  await ctx.rebuild()
  await ctx.dispose()
}

function runServerProcess() {
  return {
    name: 'run-server-process',
    setup: async function (build) {
      let abortCtl
      build.onEnd(async function (result) {
        if (result.errors.length > 0) return
        if (abortCtl) abortCtl.abort()
        let scriptPath = build.initialOptions.outfile
        console.info(`Restarting "${scriptPath}".`)
        abortCtl = forkChildProcess(scriptPath)
      })
    },
  }
}

function forkChildProcess(scriptPath) {
  const controller = new AbortController()
  fork(scriptPath, { stdio: 'inherit', signal: controller.signal })
    .on('error', error => {
      if (error.constructor.name != 'AbortError') console.error(error)
    })
    .on('close', function (code) {
      if (code) console.info(`Completed '${scriptPath}' with exit code ${code}.`)
    })
  return controller
}
