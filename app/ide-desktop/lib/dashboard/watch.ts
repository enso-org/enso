/** @file File watch and compile service. */
import * as module from 'node:module'
import * as path from 'node:path'
import * as url from 'node:url'

import chalk from 'chalk'
import * as esbuild from 'esbuild'

import * as bundler from './esbuild-config'

// =================
// === Constants ===
// =================

/** The path of this file. */
const THIS_PATH = path.resolve(path.dirname(url.fileURLToPath(import.meta.url)))
/** This must be port `8080` because it is defined as such in AWS. */
const PORT = 8080
const HTTP_STATUS_OK = 200
// `outputPath` does not have to be a real directory because `write` is `false`,
// meaning that files will not be written to the filesystem.
// However, the path should still be non-empty in order for `esbuild.serve` to work properly.
const OPTS = bundler.bundlerOptions({
  outputPath: '/',
  devMode: process.env.DEV_MODE !== 'false',
})
OPTS.define['REDIRECT_OVERRIDE'] = JSON.stringify(`http://localhost:${PORT}`)
OPTS.entryPoints.push(
  path.resolve(THIS_PATH, 'src', 'index.html'),
  path.resolve(THIS_PATH, 'src', 'entrypoint.ts'),
  path.resolve(THIS_PATH, 'src', 'serviceWorker.ts')
)
OPTS.minify = false
OPTS.write = false
OPTS.loader['.html'] = 'copy'
OPTS.pure.splice(OPTS.pure.indexOf('assert'), 1)
const REQUIRE = module.default.createRequire(import.meta.url)
OPTS.plugins.push({
  name: 'react-dom-profiling',
  setup: build => {
    build.onResolve({ filter: /^react-dom$/ }, args => {
      if (args.kind === 'import-statement') {
        return { path: REQUIRE.resolve('react-dom/profiling') }
      } else {
        return
      }
    })
  },
})

// ===============
// === Watcher ===
// ===============

/** Start the esbuild watcher. */
async function watch() {
  const builder = await esbuild.context(OPTS)
  await builder.watch()
  await builder.serve({
    port: PORT,
    servedir: OPTS.outdir,
    /** This function is called on every request.
     * It is used here to show an error if the file to serve was not found. */
    onRequest(args) {
      if (args.status !== HTTP_STATUS_OK) {
        console.error(chalk.red(`HTTP error ${args.status} when serving path '${args.path}'.`))
      }
    },
  })
}

void watch()
