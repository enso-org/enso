/** @file Types related to object paths. */

/** Checks whether T1 can be exactly (mutually) assigned to T2. */
type IsEqual<T1, T2> =
  T1 extends T2 ?
    (<G>() => G extends T1 ? 1 : 2) extends <G>() => G extends T2 ? 1 : 2 ?
      true
    : false
  : false
/** Helper function to break apart T1 and check if any are equal to T2 */
type AnyIsEqual<T1, T2> =
  T1 extends T2 ?
    IsEqual<T1, T2> extends true ?
      true
    : never
  : never
/** Helper type for recursively constructing paths through a type.
 * This actually constructs the strings and recurses into nested
 * object types. */
// Ideally, the `(V extends Constraint ? `${K}` : never)` would be part of the top level union,
// but that severely degrades typechecking performance and latency.
type PathImpl<K extends number | string, V, Constraint, TraversedTypes> =
  | (V extends (
      Date | File | FileList | bigint | boolean | number | string | symbol | null | undefined
    ) ?
      V extends Constraint ?
        `${K}`
      : never
    : true extends AnyIsEqual<TraversedTypes, V> ?
      V extends Constraint ?
        `${K}`
      : never
    : `${K}.${PathInternal<V, Constraint, TraversedTypes | V>}`)
  | (V extends Constraint ? `${K}` : never)

/** Whether an array type T is a tuple type. */
type IsTuple<T extends readonly unknown[]> = number extends T['length'] ? false : true
/** Indices of a tuple type. */
type TupleKeys<T extends readonly unknown[]> = Exclude<keyof T, keyof unknown[]>
/** Helper type for recursively constructing paths through a type.
 * This obscures the internal type param TraversedTypes from exported contract. */
type PathInternal<T, Constraint, TraversedTypes> =
  T extends ReadonlyArray<infer V> ?
    IsTuple<T> extends true ?
      {
        [K in TupleKeys<T>]-?: PathImpl<K & string, T[K], Constraint, TraversedTypes>
      }[TupleKeys<T>]
    : PathImpl<number, V, Constraint, TraversedTypes>
  : {
      [K in keyof T]-?: PathImpl<K & string, T[K], Constraint, TraversedTypes>
    }[keyof T]
/** Type which eagerly collects all paths through a type */
export type Path<T, Constraint = unknown> =
  never extends T ?
    T extends T ?
      PathInternal<T, Constraint, T>
    : never
  : never
