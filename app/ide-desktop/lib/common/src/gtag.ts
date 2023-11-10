/** @file Google Analytics tag. */

// @ts-expect-error This is explicitly not given types as it is a mistake to acess this
// anywhere else.
// eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/strict-boolean-expressions
window.dataLayer = window.dataLayer || []

/** Google Analytics tag function. */
// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function gtag(_action: 'event', _eventName: string, _eventParams?: object) {
    // @ts-expect-error This is explicitly not given types as it is a mistake to acess this
    // anywhere else.
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
    window.dataLayer.push(arguments)
}

// @ts-expect-error This signature is intentionally omitted as "event" calls should be the only
// calls allowed.
gtag('js', new Date())
// @ts-expect-error This signature is intentionally omitted as "event" calls should be the only
// calls allowed.
gtag('config', 'G-CLTBJ37MDM')
