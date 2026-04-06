/**
 * Custom ESM module loader for Playwright + Bazel integration.
 *
 * Problem: Bazel creates two node_modules paths:
 * - bin/node_modules/ (config file playwright.config.ts resolves here via symlink)
 * - runfiles/_main/node_modules/ (test files resolve here)
 *
 * Node.js treats same module at different paths as separate instances,
 * triggering Playwright's dual-instance detection.
 *
 * Solution: Register ESM hooks to rewrite bin/node_modules paths to
 * runfiles/_main/node_modules, ensuring consistent module identity.
 *
 * This file is loaded via --import flag in //app/electron-client:test Bazel rule.
 */

import { register } from 'node:module'

// Register ESM loader hooks from a separate file
register('./selective-symlink-loader-hooks.mjs', import.meta.url)
