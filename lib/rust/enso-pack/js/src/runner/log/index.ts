/** @file Logging utilities. */

export * from './logger'
export * from './router'

/** Panics with the provided message.
 * @throws Will throw an error. Use it only if there is no other option. */
export function panic(msg?: string): never {
    const suffix = msg ? ` ${msg}` : ''
    throw new Error(`Internal Error.${suffix}`)
}
