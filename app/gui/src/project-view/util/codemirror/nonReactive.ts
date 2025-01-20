/** A type like Vue's `Ref`, but without any effect tracking; see {@link pseudoRef}. */
export type PseudoRef<T> = { value: T }

export function pseudoRef<T>(value: T): PseudoRef<T>
export function pseudoRef<T>(value?: undefined): PseudoRef<T | undefined>
/**
 * Wraps a value in an object, so that it has reference semantics with regard to mutation. This supports some Vue
 * patterns, but unlike Vue's `ref` explicitly lacks any effect tracking, and so is suitable for use in logic that runs
 * without any component context, such as in CodeMirror integrations.
 */
export function pseudoRef<T>(value: T): PseudoRef<T> {
  return { value }
}
