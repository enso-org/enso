/** @file TypeScript's closest equivalent of `newtype`s. */

interface NewtypeVariant<TypeName extends string> {
    // eslint-disable-next-line @typescript-eslint/naming-convention
    _$type: TypeName
}

/** Used to create a "branded type",
 * which contains a property that only exists at compile time.
 *
 * `Newtype<string, 'A'>` and `Newtype<string, 'B'>` are not compatible with each other,
 * however both are regular `string`s at runtime.
 *
 * This is useful in parameters that require values from a certain source,
 * for example IDs for a specific object type.
 *
 * It is similar to a `newtype` in other languages.
 * Note however because TypeScript is structurally typed,
 * a branded type is assignable to its base type:
 * `a: string = asNewtype<Newtype<string, 'Name'>>(b)` successfully typechecks. */
export type Newtype<T, TypeName extends string> = NewtypeVariant<TypeName> & T

interface NotNewtype {
    // eslint-disable-next-line @typescript-eslint/naming-convention
    _$type?: never
}

export function asNewtype<T extends Newtype<unknown, string>>(
    s: NotNewtype & Omit<T, '_$type'>
): T {
    // This cast is unsafe.
    // `T` has an extra property `_$type` which is used purely for typechecking
    // and does not exist at runtime.
    //
    // The property name is specifically chosen to trigger eslint's `naming-convention` lint,
    // so it should not be possible to accidentally create a value with such a type.
    // eslint-disable-next-line no-restricted-syntax
    return s as unknown as T
}
