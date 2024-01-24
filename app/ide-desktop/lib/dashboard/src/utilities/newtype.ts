/** @file Emulates `newtype`s in TypeScript. */

// ===============
// === Newtype ===
// ===============

/** An interface specifying the variant of a newtype. */
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

/** Extracts the original type out of a {@link Newtype}.
 * Its only use is in {@link newtypeConstructor}. */
type UnNewtype<T extends Newtype<unknown, string>> = T extends infer U & NewtypeVariant<T['_$type']>
  ? U
  : NotNewtype & Omit<T, '_$type'>

/** An interface that matches a type if and only if it is not a newtype. */
export interface NotNewtype {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  _$type?: never
}

/** Converts a value that is not a newtype, to a value that is a newtype.
 * This function intentionally returns another function, to ensure that each function instance
 * is only used for one type, avoiding the de-optimization caused by polymorphic functions. */
export function newtypeConstructor<T extends Newtype<unknown, string>>() {
  // This cast is unsafe.
  // `T` has an extra property `_$type` which is used purely for typechecking
  // and does not exist at runtime.
  //
  // The property name is specifically chosen to trigger eslint's `naming-convention` lint,
  // so it should not be possible to accidentally create a value with such a type.
  // eslint-disable-next-line no-restricted-syntax
  return (s: NotNewtype & UnNewtype<T>) => s as unknown as T
}
