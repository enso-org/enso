/** Helper module for running esbuild in watch mode. */

import * as esbuild from 'esbuild'

/** Transform a given esbuild bundle option configuration into a watch configuration.
 * @param config - Configuration for the esbuild command.
 * @param onRebuild - Callback to be called after each rebuild.
 * @param inject - See [esbuild docs](https://esbuild.github.io/api/#inject).
 *
 **/
export function toWatchOptions(
    config: esbuild.BuildOptions,
    onRebuild?: () => void,
    inject?: esbuild.BuildOptions['inject']
): esbuild.BuildOptions {
    return {
        ...config,
        inject: [...(config.inject ?? []), ...(inject ?? [])],
        watch: {
            onRebuild(error, result) {
                if (error) console.error('watch build failed:', error)
                else onRebuild?.()
            },
        },
    }
}
