/** @file Module containing utility functions used throughout our Dashboard code, but that don't fit
 * anywhere else. */

export function handleEvent<T>(callback: () => Promise<T>) {
    return async (event: React.FormEvent) => {
        event.preventDefault()
        await callback()
    }
}

/** Used to create a "branded type", which is a type intersected with a `Brand<'Name'>`.
 *
 * This is useful in parameters that require values from a certain source,
 * for example IDs for a specific object type.
 *
 * It is similar to a `newtype` in other languages,
 * however because TypeScript is structurally typed, a branded type is assignable to its base type:
 * `a: string = b as (string & Brand<'Name'>)` is valid. */
export interface Brand<T extends string> {
    $brand: T
}

interface NoBrand {
    $brand?: never
}

export function brand<T extends Brand<string>>(s: NoBrand & Omit<T, '$brand'>): T {
    // <insert comment here explaining why this is safe for our purposes>
    // We are explicitly doing an unsafe cast to add a brand to the string.
    // eslint-disable-next-line no-restricted-syntax
    return s as unknown as T
}
