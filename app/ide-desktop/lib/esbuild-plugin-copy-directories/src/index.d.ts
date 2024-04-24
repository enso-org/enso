/** @file Typings for this plugin. */

// =============
// === Types ===
// =============

/** Configuration options for `esbuild-plugin-copy-directories`. */
export interface Options {
    readonly directoryFilter?: RegExp
    readonly log?: ((message: string) => void) | null
}

// ====================================
// === esbuildPluginCopyDirectories ===
// ====================================

export default function esbuildPluginCopyDirectories(options?: Options): esbuild.Plugin
