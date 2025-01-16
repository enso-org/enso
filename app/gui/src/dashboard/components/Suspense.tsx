/**
 * @file
 *
 * Suspense is a component that allows you to wrap a part of your application that might suspend,
 * showing a fallback to the user while waiting for the data to load.
 */

import * as React from 'react'

import * as loader from './Loader'

/** Props for {@link Suspense} component. */
export interface SuspenseProps extends React.PropsWithChildren {
  readonly fallback?: React.ReactNode | undefined
  readonly loaderProps?: loader.LoaderProps | undefined
}

/**
 * Suspense is a component that allows you to wrap a part of your application that might suspend,
 * showing a fallback to the user while waiting for the data to load.
 *
 * Unlike the React.Suspense component, this component does not require a fallback prop.
 * And handles offline scenarios.
 */
export function Suspense(props: SuspenseProps) {
  const { children, loaderProps, fallback } = props

  return (
    <React.Suspense fallback={<Loader {...loaderProps} fallback={fallback} />}>
      {children}
    </React.Suspense>
  )
}

/**
 * Props for {@link Loader} component.
 */
interface LoaderProps extends loader.LoaderProps {
  readonly fallback?: SuspenseProps['fallback']
}

/**
 * Fallback Element
 * Checks if ongoing network requests are happening
 * And shows either fallback(loader) or offline message
 *
 * Some request do not require active internet connection, e.g. requests to the local backend
 * So we don't want to show misleading information
 *
 * We check the fetching status in fallback component because
 * we want to know if there are ongoing requests once React renders the fallback in suspense
 */
export function Loader(props: LoaderProps) {
  const { fallback, ...loaderProps } = props

  return fallback ?? <loader.Loader minHeight="h24" size="medium" {...loaderProps} />
}
