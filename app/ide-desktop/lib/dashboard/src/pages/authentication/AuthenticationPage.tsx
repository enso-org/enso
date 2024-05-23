/** @file A styled authentication page.
 * This is a component, NOT a page, but it is here because it is related to the authentication pages
 * and nothing else. */
import * as React from 'react'

import * as aria from '#/components/aria'
import Page from '#/components/Page'
import FocusArea from '#/components/styled/FocusArea'

// ==========================
// === AuthenticationPage ===
// ==========================

/** Props for an {@link AuthenticationPage}. */
export interface AuthenticationPageProps extends Readonly<React.PropsWithChildren> {
  readonly 'data-testid'?: string
  readonly isNotForm?: boolean
  readonly title: string
  readonly footer?: React.ReactNode
  readonly onSubmit?: (event: React.FormEvent<HTMLFormElement>) => void
}

/** A styled authentication page. */
export default function AuthenticationPage(props: AuthenticationPageProps) {
  const { isNotForm = false, title, onSubmit, children, footer } = props
  const heading = (
    <aria.Heading level={1} className="self-center text-xl font-medium">
      {title}
    </aria.Heading>
  )
  const containerClasses =
    'flex w-full max-w-md flex-col gap-auth rounded-auth bg-selected-frame p-auth shadow-md'

  return (
    <Page>
      <FocusArea direction="vertical">
        {innerProps => (
          <div
            data-testid={props['data-testid']}
            className="flex min-h-screen flex-col items-center justify-center gap-auth text-sm text-primary"
            {...innerProps}
          >
            {isNotForm ? (
              <div className={containerClasses}>
                {heading}
                {children}
              </div>
            ) : (
              <form className={containerClasses} onSubmit={onSubmit}>
                {heading}
                {children}
              </form>
            )}
            {footer}
          </div>
        )}
      </FocusArea>
    </Page>
  )
}
