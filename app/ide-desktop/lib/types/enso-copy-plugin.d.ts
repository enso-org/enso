/** @file declaration copy-plugin */
import * as esbuild from 'esbuild'

declare module 'enso-copy-plugin' {
    function create(filesProvider: () => AsyncGenerator<string>): esbuild.Plugin
    export { create }
}
