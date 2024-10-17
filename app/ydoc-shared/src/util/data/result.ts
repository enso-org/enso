/**
 * @file A generic type that can either hold a value representing a successful result,
 * or an error.
 */

import { isSome, type Opt } from './opt'

/**
 * A type representing result of a function where errors are expected and recoverable.
 *
 * Usage:
 * ```ts
 * function mayFail() {
 *    // ....
 *    if (allIsGood) return Ok()
 *    else return Err("Something bad happened")
 * }
 *
 * const result = mayFail()
 * if (result.ok) console.log('Operation succesfull:', result.value)
 * else result.error.log('Operation failed')
 * ```
 *
 * In more complex cases, adding contextual information to errors may be useful - see
 * {@link withContext}.
 */
export type Result<T = undefined, E = unknown> =
  | { ok: true; value: T }
  | { ok: false; error: ResultError<E> }

/** Constructor of success {@link Result}. */
export function Ok(): Result<undefined, never>
export function Ok<T>(data: T): Result<T, never>
/** Implementation of `Ok` constructor. */
export function Ok<T>(data?: T): Result<T | undefined, never> {
  return { ok: true, value: data }
}

/** Constructor of error {@link Result}. */
export function Err<E>(error: E): Result<never, E> {
  return { ok: false, error: new ResultError(error) }
}

/** Helper function for converting optional value to {@link Result}. */
export function okOr<T, E>(data: Opt<T>, error: E): Result<T, E> {
  if (isSome(data)) return Ok(data)
  else return Err(error)
}

/** Unwraps the {@link Result} value. If the result is error, it is thrown. */
export function unwrap<T, E>(result: Result<T, E>): T {
  if (result.ok) return result.value
  else throw result.error
}

/** Unwraps the {@link Result} value. If the result is error, an alternative is returned. */
export function unwrapOr<T, A>(result: Result<T, unknown>, alternative: A): T | A {
  if (result.ok) return result.value
  else return alternative
}

/** Unwraps the {@link Result} value. If the result is error, it is logged and alternative is returned. */
export function unwrapOrWithLog<T, A>(
  result: Result<T, unknown>,
  alternative: A,
  preamble?: string,
): T | A {
  if (result.ok) return result.value
  else {
    result.error.log(preamble)
    return alternative
  }
}

/** Maps the {@link Result} value. */
export function mapOk<T, U, E>(result: Result<T, E>, f: (value: T) => U): Result<U, E> {
  if (result.ok) return Ok(f(result.value))
  else return result
}

/** If the value is nullish, returns {@link Ok} with it. */
export function transposeResult<T, E>(value: Opt<Result<T, E>>): Result<Opt<T>, E>
/** If any of the values is an error, the first error is returned. */
export function transposeResult<T, E>(value: Result<T, E>[]): Result<T[], E>
/** Implementation of `transposeResult`. */
export function transposeResult<T, E>(value: Opt<Result<T, E>> | Result<T, E>[]) {
  if (value == null) return Ok(value)
  if (value instanceof Array) {
    const error = value.find(x => !x.ok)
    if (error) return error
    else return Ok(Array.from(value, x => (x as { ok: true; value: T }).value))
  }
  return value
}

/** Check if given value is {@link Result}. */
export function isResult(v: unknown): v is Result {
  return (
    v != null &&
    typeof v === 'object' &&
    'ok' in v &&
    typeof v.ok === 'boolean' &&
    ('value' in v || ('error' in v && v.error instanceof ResultError))
  )
}

/** A class containing information about {@link Result} error. */
export class ResultError<E = unknown> {
  payload: E
  /** All contexts attached by {@link withContext} function */
  context: (() => string)[]

  /** Create an {@link ResultError}. */
  constructor(payload: E) {
    this.payload = payload
    this.context = []
  }

  /** Log the error with context information and given preable. */
  log(preamble: string = 'Error') {
    console.error(this.message(preamble))
  }

  /**
   * Returns the error message: the given preamble, the payload coerced to string and all context
   * information.
   */
  message(preamble: string = 'error') {
    const ctx =
      this.context.length > 0 ? `\n${Array.from(this.context, ctx => ctx()).join('\n')}` : ''
    return `${preamble}: ${this.payload}${ctx}`
  }
}

/**
 * Adds a context information to any error returned by `f`
 *
 * It it useful for making helpful error messages in complex operations, where result errors may be
 * propagated through several calls.
 *
 * A simplified example:
 * ```ts
 *
 * function parse(x: unknown): Result<number> {
 *   const parsed = parseFloat(x)
 *   if (isNan(parsed)) return Err(`Cannot parse ${x} as number`)
 *   return parsed
 * }
 *
 * function parseAndAdd(a: unknown, b: unknown): Result<number> {
 *   const parsedA = withContext(
 *     () => 'When parsing left operand',
 *     () => parse(a)
 *   )
 *   if (!parsedA.ok) return parsedA
 *   const parsedB = withContext(
 *     () => 'When parsing right operand',
 *     () => parse(b)
 *   )
 *   if (!parsedB.ok) return parsedB
 *   return parsedA + parsedB
 * }
 *
 * parseAndAdd('2', '3') // returns Ok(5)
 *
 * // Will print:
 * // Error: Cannot parse x as number
 * // When parsing right operand
 * const result = parseAndAdd('2', 'x')
 * result.ok || result.error.log()
 */
export function withContext<T, E>(context: () => string, f: () => Result<T, E>): Result<T, E>
export function withContext<T, E>(
  context: () => string,
  f: () => Promise<Result<T, E>>,
): Promise<Result<T, E>>
/** Implementation of `withContext`. */
export function withContext<T, E>(
  context: () => string,
  f: () => Promise<Result<T, E>> | Result<T, E>,
) {
  const result = f()
  const handleResult = (result: Result<T, E>) => {
    if (result == null) {
      throw new Error('withContext: f() returned null or undefined')
    }
    if (!result.ok) result.error.context.splice(0, 0, context)
    return result
  }
  if (result instanceof Promise) {
    return result.then(handleResult)
  } else {
    return handleResult(result)
  }
}

/** Catch promise rejection of provided types and convert them to a Result type. */
export function rejectionToResult<ErrorKind extends new (...args: any[]) => any>(
  errorKinds: ErrorKind | ErrorKind[],
): <T>(promise: Promise<T>) => Promise<Result<T, InstanceType<ErrorKind>>> {
  const errorKindArray = Array.isArray(errorKinds) ? errorKinds : [errorKinds]
  return async promise => {
    try {
      return Ok(await promise)
    } catch (error) {
      for (const errorKind of errorKindArray) {
        if (error instanceof errorKind) return Err(error)
      }
      throw error
    }
  }
}
