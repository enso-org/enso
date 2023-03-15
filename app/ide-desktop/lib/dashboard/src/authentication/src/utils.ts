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
