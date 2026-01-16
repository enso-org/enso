#!/usr/bin/env node
/**
 * Custom Playwright test runner wrapper for Bazel.
 * Uses a custom ESM loader to solve dual-module instance issues.
 *
 * Problem: Bazel creates two node_modules paths (bin/ and runfiles/).
 * Solution: Use --import flag to load a custom ESM resolver that rewrites
 * bin/ paths back to runfiles/ for source files.
 */
import { spawn } from 'node:child_process';
import path from 'node:path';

const runfilesDir = process.env.JS_BINARY__RUNFILES;
if (!runfilesDir) {
  console.error('ERROR: JS_BINARY__RUNFILES not set. Run via Bazel.');
  process.exit(1);
}

// Set NODE_PATH to runfiles node_modules
const nodeModulesPath = path.join(runfilesDir, '_main', 'node_modules');
const newNodePath = process.env.NODE_PATH
  ? `${nodeModulesPath}:${process.env.NODE_PATH}`
  : nodeModulesPath;

// Path to our custom ESM loader
const loaderPath = path.join(
  runfilesDir,
  '_main',
  'app',
  'electron-client',
  'selective-symlink-loader.mjs'
);

// Playwright CLI path in runfiles
const playwrightCli = path.join(
  nodeModulesPath,
  '.aspect_rules_js',
  'playwright@1.55.1',
  'node_modules',
  'playwright',
  'cli.js'
);

const child = spawn(
  process.execPath,
  ['--import', loaderPath, playwrightCli, ...process.argv.slice(2)],
  {
    stdio: 'inherit',
    env: { ...process.env, NODE_PATH: newNodePath },
    cwd: process.cwd(),
  }
);

child.on('exit', (code, signal) => {
  if (signal) process.kill(process.pid, signal);
  else process.exit(code ?? 1);
});
