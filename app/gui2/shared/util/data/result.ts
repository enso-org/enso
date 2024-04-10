/** @file A generic type that can either hold a value representing a successful result,
 * or an error. */

import { isSome, type Opt } from './opt'

export type Result<T = undefined, E = unknown> =
  | { ok: true; value: T }
  | { ok: false; error: ResultError<E> }

export function Ok(): Result<void, never>
export function Ok<T>(data: T): Result<T, never>
export function Ok<T>(data?: T): Result<T | undefined, never> {
  return { ok: true, value: data }
}

export function Err<E>(error: E): Result<never, E> {
  return { ok: false, error: new ResultError(error) }
}

export function okOr<T, E>(data: Opt<T>, error: E): Result<T, E> {
  if (isSome(data)) return Ok(data)
  else return Err(error)
}

export function unwrap<T, E>(result: Result<T, E>): T {
  if (result.ok) return result.value
  else throw result.error
}

export function mapOk<T, U, E>(result: Result<T, E>, f: (value: T) => U): Result<U, E> {
  if (result.ok) return Ok(f(result.value))
  else return result
}

export function isResult(v: unknown): v is Result {
  return (
    v != null &&
    typeof v === 'object' &&
    'ok' in v &&
    typeof v.ok === 'boolean' &&
    ('value' in v || ('error' in v && v.error instanceof ResultError))
  )
}

export class ResultError<E = unknown> {
  payload: E
  context: (() => string)[]

  constructor(payload: E) {
    this.payload = payload
    this.context = []
  }

  log(preamble: string = 'Error') {
    console.error(this.message(preamble))
  }

  message(preamble: string = 'error') {
    const ctx =
      this.context.length > 0 ? `\n${Array.from(this.context, (ctx) => ctx()).join('\n')}` : ''
    return `${preamble}: ${this.payload}${ctx}`
  }
}

export function withContext<T, E>(context: () => string, f: () => Result<T, E>): Result<T, E>
export function withContext<T, E>(
  context: () => string,
  f: () => Promise<Result<T, E>>,
): Promise<Result<T, E>>
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

/**
 * Catch promise rejection of provided types and convert them to a Result type.
 */
export function rejectionToResult<ErrorKind extends new (...args: any[]) => any>(
  errorKinds: ErrorKind | ErrorKind[],
): <T>(promise: Promise<T>) => Promise<Result<T, InstanceType<ErrorKind>>> {
  const errorKindArray = Array.isArray(errorKinds) ? errorKinds : [errorKinds]
  return async (promise) => {
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
