/** @file Typings for this plugin. */

export interface Options {
    directoryFilter?: RegExp
    log?: ((message: string) => void) | null
}

export default function esbuildPluginCopyDirectories(options?: Options): esbuild.Plugin
