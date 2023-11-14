/** @file Google Analytics tag. */

// @ts-expect-error This is explicitly not given types as it is a mistake to acess this
// anywhere else.
// eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/strict-boolean-expressions
window.dataLayer = window.dataLayer || []

/** Google Analytics tag function. */
// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function gtag(_action: 'config' | 'event' | 'js' | 'set', ..._args: unknown[]) {
    // @ts-expect-error This is explicitly not given types as it is a mistake to acess this
    // anywhere else.
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
    window.dataLayer.push(arguments)
}

/** Send event to Google Analytics. */
export function event(name: string, params?: object) {
    gtag('event', name, params)
}

gtag('js', new Date())
// eslint-disable-next-line @typescript-eslint/naming-convention
gtag('set', 'linker', { accept_incoming: true })
gtag('config', 'G-CLTBJ37MDM')
gtag('config', 'G-DH47F649JC')
