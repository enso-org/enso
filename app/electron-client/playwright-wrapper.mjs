#!/usr/bin/env node
/**
 * Custom Playwright test runner wrapper for Bazel.
 * Uses a custom ESM loader to solve dual-module instance issues.
 *
 * Problem: Bazel creates two node_modules paths (bin/ and runfiles/).
 * Solution: Use --import flag to load a custom ESM resolver that rewrites
 * bin/ paths back to runfiles/ for source files.
 */
import { spawn } from 'node:child_process'
import { createRequire } from 'node:module'
import path from 'node:path'

const runfilesDir = process.env.JS_BINARY__RUNFILES
if (!runfilesDir) {
  console.error('ERROR: JS_BINARY__RUNFILES not set. Run via Bazel.')
  process.exit(1)
}

// Set NODE_PATH to runfiles node_modules
const nodeModulesPath = path.join(runfilesDir, '_main', 'node_modules')
const newNodePath =
  process.env.NODE_PATH ?
    `${nodeModulesPath}${path.delimiter}${process.env.NODE_PATH}`
  : nodeModulesPath

const packageRoot = path.join(runfilesDir, '_main', 'app', 'electron-client')
const require = createRequire(import.meta.url)

// Path to our custom ESM loader
const loaderPath = path.join(
  runfilesDir,
  '_main',
  'app',
  'electron-client',
  'selective-symlink-loader.mjs',
)

// Playwright CLI path in runfiles
let playwrightCli
try {
  const playwrightPackageJson = require.resolve('playwright/package.json', {
    paths: [packageRoot],
  })
  playwrightCli = path.join(path.dirname(playwrightPackageJson), 'cli.js')
} catch (error) {
  const details = error instanceof Error ? error.message : String(error)
  console.error(`ERROR: Failed to resolve Playwright CLI: ${details}`)
  process.exit(1)
}

const child = spawn(
  process.execPath,
  ['--import', loaderPath, playwrightCli, ...process.argv.slice(2)],
  {
    stdio: 'inherit',
    env: { ...process.env, NODE_PATH: newNodePath },
    cwd: process.cwd(),
  },
)

child.on('exit', (code, signal) => {
  if (signal) process.kill(process.pid, signal)
  else process.exit(code ?? 1)
})
