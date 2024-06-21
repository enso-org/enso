/**
 * @file
 *
 * Suspense is a component that allows you to wrap a part of your application that might suspend,
 * showing a fallback to the user while waiting for the data to load.
 */

import * as React from 'react'

import * as offlineHooks from '#/hooks/offlineHooks'

import * as textProvider from '#/providers/TextProvider'

import * as result from '#/components/Result'

import * as loader from './Loader'

/**
 * Props for {@link Suspense} component.
 */
export interface SuspenseProps extends React.SuspenseProps {
  readonly loaderProps?: loader.LoaderProps
  readonly offlineFallback?: React.ReactNode
  readonly offlineFallbackProps?: result.ResultProps
}

/**
 * Suspense is a component that allows you to wrap a part of your application that might suspend,
 * showing a fallback to the user while waiting for the data to load.
 *
 * Unlike the React.Suspense component, this component does not require a fallback prop.
 * And handles offline scenarios.
 */
export function Suspense(props: SuspenseProps) {
  const { children, loaderProps, fallback, offlineFallbackProps, offlineFallback } = props

  const { getText } = textProvider.useText()
  const { isOffline } = offlineHooks.useOffline()

  const getFallbackElement = () => {
    if (isOffline) {
      return (
        offlineFallback ?? (
          <result.Result status="info" title={getText('offlineTitle')} {...offlineFallbackProps} />
        )
      )
    } else {
      return fallback ?? <loader.Loader minHeight="h24" size="medium" {...loaderProps} />
    }
  }

  return <React.Suspense fallback={getFallbackElement()}>{children}</React.Suspense>
}
