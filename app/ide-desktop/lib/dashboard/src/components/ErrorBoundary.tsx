/** @file Catches errors in child components. */
import * as React from 'react'

import * as sentry from '@sentry/react'
import * as reactQuery from '@tanstack/react-query'
import * as errorBoundary from 'react-error-boundary'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import * as result from '#/components/Result'

// =====================
// === ErrorBoundary ===
// =====================

/** Props for an {@link ErrorBoundary}. */
export interface ErrorBoundaryProps
  extends Readonly<React.PropsWithChildren>,
    Readonly<Pick<errorBoundary.ErrorBoundaryProps, 'FallbackComponent' | 'onError' | 'onReset'>> {}

/** Catches errors in the child components
 * Shows a fallback UI when there is an error.
 * The error can also be logged. to an error reporting service. */
export function ErrorBoundary(props: ErrorBoundaryProps) {
  const {
    FallbackComponent = DefaultFallbackComponent,
    onError = () => {},
    onReset = () => {},
    ...rest
  } = props
  return (
    <reactQuery.QueryErrorResetBoundary>
      {({ reset }) => (
        <errorBoundary.ErrorBoundary
          FallbackComponent={FallbackComponent}
          onError={(error, info) => {
            sentry.captureException(error, { extra: { info } })
            onError(error, info)
          }}
          onReset={details => {
            reset()
            onReset(details)
          }}
          {...rest}
        />
      )}
    </reactQuery.QueryErrorResetBoundary>
  )
}

/** Default fallback component to show when there is an error. */
function DefaultFallbackComponent(props: errorBoundary.FallbackProps): React.JSX.Element {
  const { resetErrorBoundary } = props

  const { getText } = textProvider.useText()

  return (
    <result.Result
      className="h-full"
      status="error"
      title={getText('arbitraryErrorTitle')}
      subtitle={getText('arbitraryErrorSubtitle')}
    >
      <ariaComponents.ButtonGroup align="center">
        <ariaComponents.Button variant="submit" size="medium" onPress={resetErrorBoundary}>
          {getText('tryAgain')}
        </ariaComponents.Button>
      </ariaComponents.ButtonGroup>
    </result.Result>
  )
}

export { useErrorBoundary, withErrorBoundary } from 'react-error-boundary'
