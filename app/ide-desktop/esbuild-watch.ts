/** Helper module for running esbuild in watch mode. */

import * as esbuild from 'esbuild'

/** Make a watcher for a given esbuild bundle option configuration.
 * @param config - Configuration for the esbuild command.
 * @param onRebuild - Callback to be called after each rebuild.
 * @param inject - See [esbuild docs](https://esbuild.github.io/api/#inject).
 *
 **/
export async function watch(config: esbuild.BuildOptions, onRebuild?: () => void, inject?: esbuild.BuildOptions['inject']) {
    return esbuild.build({
        ...config,
        inject: [...(config.inject ?? []), ...(inject ?? [])],
        watch: {
            onRebuild(error, result) {
                if (error) console.error('watch build failed:', error)
                else onRebuild?.()
            },
        },
    })
}
