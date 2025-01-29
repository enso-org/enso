/**
 * @file
 *
 * Await a promise and render the children when the promise is resolved.
 */
import { type ReactNode } from 'react'

import invariant from 'tiny-invariant'
import { ErrorBoundary, type ErrorBoundaryProps } from './ErrorBoundary'
import { Suspense, type SuspenseProps } from './Suspense'

/**
 * Props for the {@link Await} component.
 */
export interface AwaitProps<PromiseType>
  extends Omit<SuspenseProps, 'children'>,
    Omit<ErrorBoundaryProps, 'children'> {
  /**
   * Promise to await.
   *
   * ___The promise instance ***must be stable***, otherwise this will lock the UI into the loading state___
   */
  readonly promise: Promise<PromiseType>
  readonly children: ReactNode | ((value: PromiseType) => ReactNode)
}

/**
 * State of the promise.
 */
export type PromiseState<T> =
  | {
      readonly status: 'error'
      readonly data?: never
      readonly error: unknown
    }
  | {
      readonly status: 'pending'
      readonly data?: never
      readonly error?: never
    }
  | {
      readonly status: 'success'
      readonly data: T
      readonly error?: never
    }

/**
 * Awaits a promise and render the children when the promise resolves.
 * Works well with React Query, as it returns a cached promise from the useQuery hook.
 * Useful to trigger Suspense ***inside*** the component, rather than ***outside*** of it.
 * @example
 * const {promise} = useQuery({queryKey: ['data'], queryFn: fetchData})
 *
 * <Await promise={promise}>
 *   {(data) => <div>{data}</div>}
 * </Await>
 */
export function Await<PromiseType>(props: AwaitProps<PromiseType>) {
  const {
    promise,
    children,
    FallbackComponent,
    fallback,
    loaderProps,
    onBeforeFallbackShown,
    onError,
    onReset,
    resetKeys,
    subtitle,
    title,
  } = props

  return (
    <ErrorBoundary
      FallbackComponent={FallbackComponent}
      onError={onError}
      onBeforeFallbackShown={onBeforeFallbackShown}
      onReset={onReset}
      resetKeys={resetKeys}
      subtitle={subtitle}
      title={title}
    >
      <Suspense fallback={fallback} loaderProps={loaderProps}>
        <AwaitInternal promise={promise} children={children} />
      </Suspense>
    </ErrorBoundary>
  )
}

const PRIVATE_AWAIT_PROMISE_STATE = Symbol('PRIVATE_AWAIT_PROMISE_STATE_REF')

/**
 * Internal implementation of the {@link Await} component.
 *
 * This component throws the promise and trigger the Suspense boundary
 * inside the {@link Await} component.
 * @throws {Promise} - The promise that is being awaited by Suspense.
 */
function AwaitInternal<PromiseType>(props: AwaitProps<PromiseType>) {
  const { promise, children } = props

  const data = useAwait(promise)

  return typeof children === 'function' ? children(data) : children
}

export function useAwait(promise?: null): void
export function useAwait<PromiseType>(promise: Promise<PromiseType>): PromiseType
export function useAwait<PromiseType>(
  promise?: Promise<PromiseType> | null,
): PromiseType | undefined

/**
 * A hook that accepts a promise and triggers the Suspense boundary until the promise is resolved.
 * @param promise - The promise to await.
 * @throws {Promise} - The promise that is being awaited by Suspense
 * @returns The data of the promise.
 */
export function useAwait<PromiseType>(
  promise?: Promise<PromiseType> | null,
): PromiseType | undefined {
  if (promise == null) {
    return
  }

  /**
   * Define the promise state on the promise.
   */
  const definePromiseState = (
    promiseToDefineOn: Promise<PromiseType>,
    promiseState: PromiseState<PromiseType>,
  ) => {
    // @ts-expect-error: we know that the promise state is not defined in the type but it's fine,
    // because it's a private and scoped to the component.
    promiseToDefineOn[PRIVATE_AWAIT_PROMISE_STATE] = promiseState
  }

  // We need to define the promise state, only once.
  // We don't want to use refs on state, because it scopes the state to the component.
  // But we might use multiple Await components with the same promise.
  if (!(PRIVATE_AWAIT_PROMISE_STATE in promise)) {
    definePromiseState(promise, { status: 'pending' })

    // This breaks the chain of promises, but it's fine,
    // because this is suppsed to the last in the chain.
    // and the error will be thrown in the render phase
    // to trigger the error boundary.
    void promise.then((data) => {
      definePromiseState(promise, { status: 'success', data })
    })
    void promise.catch((error) => {
      definePromiseState(promise, { status: 'error', error })
    })
  }

  // This should never happen, as the promise state is defined above.
  // But we need to check it, because the promise state is not defined in the type.
  // And we want to make TypeScript happy.
  invariant(
    PRIVATE_AWAIT_PROMISE_STATE in promise,
    'Promise state is not defined. This should never happen.',
  )

  const promiseState =
    // This is safe, as we defined the promise state above.
    // and it always present in the promise object.
    // eslint-disable-next-line no-restricted-syntax
    promise[PRIVATE_AWAIT_PROMISE_STATE] as PromiseState<PromiseType>

  if (promiseState.status === 'pending') {
    // Throwing a promise is the valid way to trigger Suspense
    // eslint-disable-next-line @typescript-eslint/only-throw-error
    throw promise
  }

  if (promiseState.status === 'error') {
    throw promiseState.error
  }

  return promiseState.data
}
