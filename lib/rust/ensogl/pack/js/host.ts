/** Resolves to `true` if we are running the script in the browser. If it's node, resolves to
 * `false`. */
const browser = typeof window !== 'undefined'

/** Resolves to `true` if we are running the script in node. If it's a browser, resolves to
 * `false`. */
const node = !browser

/** The global object. In case of a browser, this is the window. In case of node, it's the built-in
 * `global` object. */
// const global = {}
global ??= window

export default {
    global,
    browser,
    node,
}
