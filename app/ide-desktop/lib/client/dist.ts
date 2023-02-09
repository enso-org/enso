/** This script creates a packaged IDE distribution.
 *
 *  Behaviour details are controlled by the environment variables.
 *  @see Arguments
 **/

import {args, buildPackage} from './electron-builder-config.js'

await buildPackage(args)
