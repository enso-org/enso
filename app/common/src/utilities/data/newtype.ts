/** @file Emulates `newtype`s in TypeScript. */

// ===============
// === Newtype ===
// ===============

/** An interface specifying the variant of a newtype. */
type NewtypeVariant<TypeName extends string> = {
  readonly _$type: TypeName
}

/**
 * An interface specifying the variant of a newtype, where the discriminator is mutable.
 * This is safe, as the discriminator should be a string literal type anyway.
 */
type MutableNewtypeVariant<TypeName extends string> = {
  _$type: TypeName
}

/**
 * Used to create a "branded type",
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
 * `a: string = asNewtype<Newtype<string, 'Name'>>(b)` successfully typechecks.
 */
export type Newtype<T, TypeName extends string> = NewtypeVariant<TypeName> & T

/**
 * Extracts the original type out of a {@link Newtype}.
 * Its only use is in {@link newtypeConstructor}.
 */
type UnNewtype<T extends Newtype<unknown, string>> =
  T extends infer U & NewtypeVariant<T['_$type']> ?
    U extends infer V & MutableNewtypeVariant<T['_$type']> ?
      V
    : U
  : NotNewtype & Omit<T, '_$type'>

/** An interface that matches a type if and only if it is not a newtype. */
type NotNewtype = {
  readonly _$type?: never
}

/**
 * Converts a value that is not a newtype, to a value that is a newtype.
 * This function intentionally returns another function, to ensure that each function instance
 * is only used for one type, avoiding the de-optimization caused by polymorphic functions.
 */
export function newtypeConstructor<T extends Newtype<unknown, string>>() {
  // This cast is unsafe.
  // `T` has an extra property `_$type` which is used purely for typechecking
  // and does not exist at runtime.
  //
  // The property name is specifically chosen to trigger eslint's `naming-convention` lint,
  // so it should not be possible to accidentally create a value with such a type.
  return (s: NotNewtype & UnNewtype<T>) => s as unknown as T
}
