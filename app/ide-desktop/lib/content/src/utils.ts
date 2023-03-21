/** @file Module containing utility functions used throughout this module,
 * but don't fit anywhere else. */

/** Used to create a "branded type", which is a type intersected with a `Brand<'Name'>`.
 *
 * This is useful in parameters that require values from a certain source,
 * for example IDs for a specific object type.
 *
 * It is similar to a `newtype` in other languages,
 * however because TypeScript is structurally typed, a branded type is assignable to its base type:
 * `a: string = b as (string & Brand<'Name'>)` successfully typechecks. */
export interface Newtype<T extends string> {
	// eslint-disable-next-line @typescript-eslint/naming-convention
	_$type: T
}

interface NotNewtype {
	// eslint-disable-next-line @typescript-eslint/naming-convention
	_$type?: never
}

export function asNewtype<T extends Newtype<string>>(s: NotNewtype & Omit<T, '_$type'>): T {
	// This cast is unsafe. `T` has an extra property `_$type`.
	// However, this should not lead to logic errors as the property name
	// is chosen to be very difficult to accidentally use,
	// and it is *more* type-safe as there is an extra property which must also match.
	// This is reinforced by eslint's `naming-convention` lint.
	// eslint-disable-next-line no-restricted-syntax
	return s as unknown as T
}
