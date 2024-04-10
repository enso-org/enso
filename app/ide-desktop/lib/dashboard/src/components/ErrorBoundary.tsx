/**
 * @file
 *
 * ErrorBoundary component to catch errors in the child components
 */
import * as React from 'react'

import * as errorBoundary from 'react-error-boundary'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/AriaComponents'
import * as result from '#/components/Result'

/**
 * Props for the ErrorBoundary component
 */
export interface ErrorBoundaryProps {
  readonly children?: React.ReactNode
  readonly onError?: errorBoundary.ErrorBoundaryProps['onError']
  readonly onReset?: errorBoundary.ErrorBoundaryProps['onReset']
  // Field comes from an external library and we don't want to change the name
  // eslint-disable-next-line @typescript-eslint/naming-convention
  readonly FallbackComponent?: errorBoundary.ErrorBoundaryProps['FallbackComponent']
}

/**
 * ErrorBoundary component to catch errors in the child components
 * Shows a fallback UI when there is an error
 * You can also log the error to an error reporting service
 */
export function ErrorBoundary(props: ErrorBoundaryProps) {
  const {
    FallbackComponent = DefaultFallbackComponent,
    onError = () => {},
    onReset = () => {},
    ...rest
  } = props
  return (
    <errorBoundary.ErrorBoundary
      FallbackComponent={FallbackComponent}
      onError={onError}
      onReset={onReset}
      {...rest}
    />
  )
}

/**
 * Default fallback component to show when there is an error
 */
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
      <aria.Button variant="submit" onPress={resetErrorBoundary}>
        {getText('tryAgain')}
      </aria.Button>
    </result.Result>
  )
}

// Re-exporting the ErrorBoundary component
// eslint-disable-next-line no-restricted-syntax
export { useErrorBoundary, withErrorBoundary } from 'react-error-boundary'
