/** @file Typings for this plugin. */

// =============
// === Types ===
// =============

/** Configuration options for `esbuild-plugin-copy-directories`. */
export interface Options {
    directoryFilter?: RegExp
    log?: ((message: string) => void) | null
}

// ====================================
// === esbuildPluginCopyDirectories ===
// ====================================

export default function esbuildPluginCopyDirectories(options?: Options): esbuild.Plugin
