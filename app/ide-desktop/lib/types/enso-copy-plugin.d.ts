/** @file declaration copy-plugin */

declare module 'enso-copy-plugin' {
    function create(files_provider: () => AsyncGenerator<string>): import('esbuild').Plugin
    export { create }
}
