/** @file Typings for this plugin. */

/** Configuration options for `esbuild-plugin-copy-directories`. */
export interface Options {
    directoryFilter?: RegExp
    log?: ((message: string) => void) | null
}

export default function esbuildPluginCopyDirectories(options?: Options): esbuild.Plugin
