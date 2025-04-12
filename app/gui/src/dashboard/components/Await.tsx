/**
 * @file
 *
 * Await a promise and render the children when the promise is resolved.
 */
import { use, type ReactNode } from 'react'

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

/**
 * Internal implementation of the {@link Await} component.
 *
 * This component throws the promise and trigger the Suspense boundary
 * inside the {@link Await} component.
 * @throws {Promise} - The promise that is being awaited by Suspense.
 */
function AwaitInternal<PromiseType>(props: AwaitProps<PromiseType>) {
  const { promise, children } = props

  const data = use(promise)

  return typeof children === 'function' ? children(data) : children
}
