/** @file Declare all globally missing variables.
 *
 * React CLI has a built-in plugin for resolving import of *.jpg images,
 * so we can directly import and use those images.
 * However, TypeScript is not aware of this, so we need to declare it.
 */
declare module '*.jpg' {
    const url: string
    export default url
}
