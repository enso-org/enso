/**
 * @file This script creates a packaged IDE distribution using electron-builder.
 * Behaviour details are controlled by the environment variables or CLI arguments.
 * @see Arguments
 */

import * as electronBuilderConfig from './electron-builder-config.js'

await electronBuilderConfig.buildPackage(electronBuilderConfig.args)
