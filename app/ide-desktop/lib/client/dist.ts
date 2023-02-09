/** This script creates a packaged IDE distribution.
 *
 *  Behaviour details are controlled by the environment variables.
 **/

import {args, buildPackage, createElectronBuilderConfig} from './electron-builder-config.js'

await buildPackage(args)
