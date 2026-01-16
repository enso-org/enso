/**
 * ESM loader hooks for Bazel + Playwright integration.
 * This file is registered via module.register() from selective-symlink-loader.mjs
 */

import { fileURLToPath, pathToFileURL } from 'node:url';
import fs from 'node:fs';
import path from 'node:path';

// Get the runfiles node_modules path from the main loader
// This is set by the main loader before registering this hooks file
let runfilesNodeModules = null;

export function initialize(data) {
  // Get runfiles path from environment since globalThis isn't shared
  const runfilesDir = process.env.JS_BINARY__RUNFILES;
  runfilesNodeModules = runfilesDir
    ? path.join(runfilesDir, '_main', 'node_modules')
    : null;
}

/**
 * Helper function to rewrite bin/node_modules paths to runfiles
 */
function rewriteToRunfiles(resolved) {
  if (!runfilesNodeModules || !resolved.includes('/bin/node_modules/')) {
    return null;
  }

  const match = resolved.match(/\/bin\/node_modules\/(.+)$/);
  if (!match) {
    return null;
  }

  const relativePath = match[1];
  const rewritten = path.join(runfilesNodeModules, relativePath);

  try {
    fs.accessSync(rewritten);
    return rewritten;
  } catch {
    return null;
  }
}

/**
 * ESM resolve hook - intercepts module resolution
 */
export async function resolve(specifier, context, nextResolve) {
  const result = await nextResolve(specifier, context);

  if (result.url && result.url.startsWith('file://')) {
    const filePath = fileURLToPath(result.url);
    const rewritten = rewriteToRunfiles(filePath);
    if (rewritten) {
      const rewrittenUrl = pathToFileURL(rewritten).href;
      return { ...result, url: rewrittenUrl };
    }
  }

  return result;
}
