/** @file A helper module for running esbuild in watch mode. */

import * as esbuild from 'esbuild'

/** Transform a given esbuild bundle option configuration into a watch configuration.
 * @param config - Configuration for the esbuild command.
 * @param onRebuild - Callback to be called after each rebuild.
 * @param inject - See [esbuild docs](https://esbuild.github.io/api/#inject).
 */
export function toWatchOptions<T extends esbuild.BuildOptions>(
    config: T,
    onRebuild?: () => void,
    inject?: esbuild.BuildOptions['inject']
) {
    return {
        ...config,
        inject: [...(config.inject ?? []), ...(inject ?? [])],
        watch: {
            onRebuild(error) {
                if (error) console.error('watch build failed:', error)
                else onRebuild?.()
            },
        },
    } satisfies esbuild.BuildOptions
}
