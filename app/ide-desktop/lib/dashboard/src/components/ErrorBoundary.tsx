/**
 * @file
 *
 * ErrorBoundary component to catch errors in the child components
 */
import * as React from 'react'

import * as errorBoundary from 'react-error-boundary'

import Close from 'enso-assets/close.svg'

import * as aria from '#/components/AriaComponents'
import SvgMask from '#/components/SvgMask'

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
  return (
    <div className="flex h-full min-h-5 w-full flex-col items-center justify-center">
      <SvgMask src={Close} className="mb-4 h-16 w-16 flex-none text-red-800" />
      <h1 className="mb-2 text-2xl">Something went wrong</h1>
      <p className="mb-8 text-lg">There was an error in the application</p>
      <aria.Button variant="submit" onPress={resetErrorBoundary}>
        Try again
      </aria.Button>
    </div>
  )
}

// Re-exporting the ErrorBoundary component
// eslint-disable-next-line no-restricted-syntax
export { useErrorBoundary, withErrorBoundary } from 'react-error-boundary'
