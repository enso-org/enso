/** @file This script creates a packaged IDE distribution using electron-builder.
 * Behaviour details are controlled by the environment variables or CLI arguments.
 * @see electronBuilderConfig.Arguments
 */

import * as electronBuilderConfig from './electron-builder-config'

// ==============================
// === Build Electron package ===
// ==============================

await electronBuilderConfig.buildPackage(electronBuilderConfig.args)
