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
    // This cast is unsafe. It is possible to use this method to cast a value from a base type to a
    // branded type, even if that value is not an instance of the branded type. For example, the
    // string "foo" could be cast to the `UserPoolId` branded type, although this string is clearly
    // not a valid `UserPoolId`. This is acceptable because the branded type is only used to prevent
    // accidental misuse of values, and not to enforce correctness. That is, it is up to the
    // programmer to declare the correct type of a value. After that point, it is up to the branded
    // type to keep that guarantee by preventing accidental misuse of the value.
    // eslint-disable-next-line no-restricted-syntax
    return s as unknown as T
}
