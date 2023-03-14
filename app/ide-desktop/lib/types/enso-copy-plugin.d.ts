/** @file declaration copy-plugin */
declare module 'enso-copy-plugin' {
    import * as esbuild from 'esbuild'

    export function create(filesProvider: () => AsyncGenerator<string>): esbuild.Plugin
}
