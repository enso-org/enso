/** @file Catches errors in child components. */
import Offline from '#/assets/offline_filled.svg'
import { Alert } from '#/components/Alert'
import { Button, ButtonGroup } from '#/components/Button'
import { Icon } from '#/components/Icon'
import { Result, type ResultProps } from '#/components/Result'
import { Separator } from '#/components/Separator'
import SvgMask from '#/components/SvgMask'
import { Text } from '#/components/Text'
import type { SvgUseIcon } from '#/components/types'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useText } from '$/providers/react'
import * as sentry from '@sentry/vue'
import * as reactQuery from '@tanstack/react-query'
import * as detect from 'enso-common/src/utilities/detect'
import * as errorUtils from 'enso-common/src/utilities/errors'
import * as React from 'react'
import type { FallbackProps } from 'react-error-boundary'
import * as errorBoundary from 'react-error-boundary'

/** Arguments for the {@link ErrorBoundaryProps.onBeforeFallbackShown} callback. */
export interface OnBeforeFallbackShownArgs {
  readonly error: unknown
  readonly resetErrorBoundary: () => void
  readonly resetQueries: () => void
}

/** Props for an {@link ErrorBoundary}. */
export interface ErrorBoundaryProps extends Readonly<React.PropsWithChildren> {
  /** Keys to reset the error boundary. Use it to declaratively reset the error boundary. */
  readonly resetKeys?: errorBoundary.ErrorBoundaryProps['resetKeys'] | undefined
  /** Fallback component to show when there is an error. */
  // This is a Component, and supposed to be capitalized according to the react conventions.
  // eslint-disable-next-line @typescript-eslint/naming-convention
  readonly FallbackComponent?: React.ComponentType<FallbackProps> | undefined
  /** Called when there is an error. */
  readonly onError?: errorBoundary.ErrorBoundaryProps['onError'] | undefined
  /** Called when the error boundary is reset. */
  readonly onReset?: errorBoundary.ErrorBoundaryProps['onReset'] | undefined
  /**
   * Called before the fallback is shown, can return a React node to render instead of the fallback.
   * Alternatively, you can use the error boundary api to reset the error boundary based on the error.
   */
  readonly onBeforeFallbackShown?:
    | ((args: OnBeforeFallbackShownArgs) => React.ReactNode | null | undefined)
    | undefined
  /** Title to show when there is an error. */
  readonly title?: string | undefined
  /** Subtitle to show when there is an error. */
  readonly subtitle?: string | undefined
}

/**
 * Catches errors in child components
 * Shows a fallback UI when there is an error.
 * The error can also be logged to an error reporting service.
 */
export function ErrorBoundary(props: ErrorBoundaryProps) {
  const {
    FallbackComponent = ErrorDisplay,
    onError = () => {},
    onReset = () => {},
    onBeforeFallbackShown = () => null,
    title,
    subtitle,
    resetKeys,
    ...rest
  } = props

  return (
    <reactQuery.QueryErrorResetBoundary>
      {({ reset }) => (
        <errorBoundary.ErrorBoundary
          {...(resetKeys != null ? { resetKeys } : {})}
          FallbackComponent={(fallbackProps) => {
            const displayMessage = errorUtils.extractDisplayMessage(fallbackProps.error)

            return (
              <FallbackComponent
                {...fallbackProps}
                onBeforeFallbackShown={onBeforeFallbackShown}
                resetQueries={reset}
                title={title}
                subtitle={subtitle ?? displayMessage ?? null}
              />
            )
          }}
          onError={(error, info) => {
            sentry.captureException(error, { extra: { info } })
            onError(error, info)
          }}
          onReset={(details) => {
            reset()
            onReset(details)
          }}
          {...rest}
        />
      )}
    </reactQuery.QueryErrorResetBoundary>
  )
}

/** Props for a {@link ErrorDisplay}. */
export interface ErrorDisplayProps extends errorBoundary.FallbackProps {
  readonly status?: ResultProps['status']
  readonly onBeforeFallbackShown?: (args: OnBeforeFallbackShownArgs) => React.ReactNode | undefined
  readonly resetQueries?: () => void
  readonly title?: string | null | undefined
  readonly subtitle?: string | null | undefined
  readonly error: unknown
}

/** Default fallback component to show when there is an error. */
export function ErrorDisplay(props: ErrorDisplayProps): React.JSX.Element {
  const { getText } = useText()

  const {
    error,
    resetErrorBoundary,
    title,
    subtitle,
    status,
    onBeforeFallbackShown,
    resetQueries = () => {},
  } = props

  const isOfflineError = error instanceof errorUtils.OfflineError

  const message = errorUtils.getMessageOrToString(error)
  const stack = errorUtils.tryGetStack(error)

  const render = onBeforeFallbackShown?.({ error, resetErrorBoundary, resetQueries })

  const onReset = useEventCallback(() => {
    resetErrorBoundary()
  })

  const finalTitle = title ?? getText('somethingWentWrong')
  const finalSubtitle =
    subtitle ??
    (isOfflineError ? getText('offlineErrorMessage') : getText('arbitraryErrorSubtitle'))
  const finalStatus =
    status ?? (isOfflineError ? <SvgMask src={Offline} className="aspect-square w-6" /> : 'error')

  const defaultRender = (
    <Result
      className="h-full"
      status={finalStatus}
      title={finalTitle}
      subtitle={finalSubtitle}
      testId="error-display"
    >
      <ButtonGroup align="center">
        <Button variant="submit" size="small" rounded="full" className="w-24" onPress={onReset}>
          {getText('tryAgain')}
        </Button>
      </ButtonGroup>

      {detect.IS_DEV_MODE && stack != null && (
        <div className="mt-6">
          <Separator className="my-2" />

          <Text color="primary" variant="h1" className="text-start">
            {getText('developerInfo')}
          </Text>

          <Text color="danger" variant="body">
            {getText('errorColon')}
            {message}
          </Text>

          <Alert
            className="mx-auto mt-2 max-h-[80vh] max-w-screen-lg overflow-auto"
            variant="neutral"
          >
            <Text
              elementType="pre"
              className="whitespace-pre-wrap text-left"
              color="primary"
              variant="body"
            >
              {stack}
            </Text>
          </Alert>
        </div>
      )}
    </Result>
  )

  return <>{render ?? defaultRender}</>
}

/** Props for an {@link InlineErrorDisplay}. */
export interface InlineErrorDisplayProps extends Omit<ErrorDisplayProps, 'status' | 'subtitle'> {}

/** Displays an error inline. */
export function InlineErrorDisplay(props: InlineErrorDisplayProps) {
  const { error, resetErrorBoundary, onBeforeFallbackShown, title, resetQueries = () => {} } = props

  const { getText } = useText()

  const render = onBeforeFallbackShown?.({ error, resetErrorBoundary, resetQueries })

  const onReset = useEventCallback(() => {
    resetErrorBoundary()
  })

  const finalTitle = title ?? getText('somethingWentWrong')
  const finalIcon: SvgUseIcon = 'error'

  const defaultRender = (
    <div className="flex items-center gap-1">
      <Icon icon={finalIcon} />
      <Text>{finalTitle}</Text>
      <Button variant="outline" size="xsmall" onPress={onReset} className="ml-3">
        {getText('tryAgain')}
      </Button>
    </div>
  )

  return <>{render ?? defaultRender}</>
}

// eslint-disable-next-line react-refresh/only-export-components
export { useErrorBoundary, withErrorBoundary } from 'react-error-boundary'
