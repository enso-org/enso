/** @file A wrapper for a twemoji image. */
import * as React from 'react'

// =================
// === Constants ===
// =================

/** The base of hexadecimal numbers. */
const HEXADECIMAL = 16

// ===============
// === Twemoji ===
// ===============

/** Props for a {@link Twemoji}. */
export interface TwemojiProps {
    emoji: string
}

// Only accepts strings that are two code points - for example, emojis.
/** Returns the input type if it consists of two codepoints. Otherwise, it returns
 * an error message. */
type MustBeLength2String<T extends string> = T extends `${string}${string}${infer Rest}`
    ? Rest extends ''
        ? T
        : 'Error: string must have a length of 2'
    : 'Error: string must have a length of 2'

/** Props for a {@link Twemoji}, but with extra validation. */
interface InternalValidTwemojiProps<T extends string> {
    emoji: MustBeLength2String<T>
    size: number
}

/** Serves a Twemoji image from the JSDelivr CDN. */
export default function Twemoji<T extends string>(props: InternalValidTwemojiProps<T>) {
    const { emoji, size } = props

    // This is safe as the that the string is required to be non-empty by the type of `props`.
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const imagePath = emoji.codePointAt(0)!.toString(HEXADECIMAL)

    return (
        <img
            src={`https://cdn.jsdelivr.net/npm/twemoji@latest/2/svg/${imagePath}.svg`}
            crossOrigin="anonymous"
            height={size}
            width={size}
            alt={emoji}
        />
    )
}
