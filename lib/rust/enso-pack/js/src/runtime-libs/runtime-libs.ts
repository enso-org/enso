/** @file Contains {@link spector} function which is a wrapper for
 * [spectorjs]{@link https://github.com/BabylonJS/Spector.js}. */

/* eslint @typescript-eslint/no-unsafe-return: "off" */
/** Spectorjs function wrapper. */
export function spector() {
    return require('spectorjs')
}
