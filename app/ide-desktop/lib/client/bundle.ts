/** Script that bundles JS client code. */

import path from 'node:path'
import esbuild from 'esbuild'
import { bundlerOptionsFromEnv } from './esbuild-config.js'

const bundlerOptions: esbuild.BuildOptions = bundlerOptionsFromEnv()
await esbuild.build(bundlerOptions)
