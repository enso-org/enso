/**
 * Custom ESM module loader for Playwright + Bazel integration.
 *
 * Problem: Bazel creates two node_modules paths:
 * - bin/node_modules/ (config file resolves here via symlink)
 * - runfiles/_main/node_modules/ (test files resolve here)
 *
 * Node.js treats same module at different paths as separate instances,
 * triggering Playwright's dual-instance detection.
 *
 * Solution: Register ESM hooks to rewrite bin/node_modules paths to
 * runfiles/_main/node_modules, ensuring consistent module identity.
 *
 * This file is loaded via --import flag which runs before the main entry point.
 *
 * Note: CJS hook is not needed when tests use "type": "module" since Playwright
 * uses ESM resolution for ESM projects.
 */

import { register } from 'node:module';

// Register ESM loader hooks from a separate file
register('./selective-symlink-loader-hooks.mjs', import.meta.url);
