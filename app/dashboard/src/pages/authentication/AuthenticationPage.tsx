/** @file A styled authentication page.
 * This is a component, NOT a page, but it is here because it is related to the authentication pages
 * and nothing else. */
import * as React from 'react'

import * as offlineHooks from '#/hooks/offlineHooks'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import Page from '#/components/Page'

// ==========================
// === AuthenticationPage ===
// ==========================

/** Props for an {@link AuthenticationPage}. */
export interface AuthenticationPageProps extends Readonly<React.PropsWithChildren> {
  readonly supportsOffline?: boolean
  readonly 'data-testid'?: string
  readonly title: string
  readonly footer?: React.ReactNode
  readonly onSubmit?: (event: React.FormEvent<HTMLFormElement>) => void
}

/** A styled authentication page. */
export default function AuthenticationPage(props: AuthenticationPageProps) {
  const { title, onSubmit, children, footer, supportsOffline = false } = props
  const isForm = onSubmit != null

  const { getText } = textProvider.useText()
  const { isOffline } = offlineHooks.useOffline()

  const heading = (
    <ariaComponents.Text.Heading level={1} className="self-center" weight="medium">
      {title}
    </ariaComponents.Text.Heading>
  )

  const containerClasses = ariaComponents.DIALOG_BACKGROUND({
    className: 'flex w-full flex-col gap-auth rounded-4xl p-12',
  })

  const offlineAlertClasses = ariaComponents.DIALOG_BACKGROUND({
    className: 'flex mt-auto rounded-sm items-center justify-center p-4 px-12 rounded-4xl',
  })

  return (
    <Page>
      <div className="flex h-full w-full flex-col overflow-y-auto p-12">
        <div
          className="relative m-auto grid h-full w-full max-w-md grid-cols-1 grid-rows-[1fr_auto_1fr] flex-col items-center justify-center gap-auth text-sm text-primary"
          data-testid={props['data-testid']}
        >
          {isOffline && (
            <div className={offlineAlertClasses}>
              <ariaComponents.Text className="text-center" balance elementType="p">
                {getText('loginUnavailableOffline')}{' '}
                {supportsOffline && getText('loginUnavailableOfflineLocal')}
              </ariaComponents.Text>
            </div>
          )}

          <div className="row-start-2 row-end-3 flex w-full flex-col items-center gap-auth">
            {!isForm ?
              <div className={containerClasses}>
                {heading}
                {children}
              </div>
            : <form
                className={containerClasses}
                onSubmit={(event) => {
                  event.preventDefault()
                  onSubmit(event)
                }}
              >
                {heading}
                {children}
              </form>
            }
            {footer}
          </div>
        </div>
      </div>
    </Page>
  )
}
