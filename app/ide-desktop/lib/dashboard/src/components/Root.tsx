/** @file The root component with required providers */
import * as React from 'react'

import * as aria from '#/components/aria'
import * as portal from '#/components/Portal'

/** Props for {@link Root}. */
export interface RootProps extends React.PropsWithChildren {
  readonly rootRef: React.RefObject<HTMLElement>
  readonly navigate: (path: string) => void
  readonly locale?: string
}

/** The root component with required providers. */
export function Root(props: RootProps) {
  const { children, rootRef, navigate, locale = 'en-US' } = props

  return (
    <portal.PortalProvider value={rootRef}>
      <aria.RouterProvider navigate={navigate}>
        <aria.I18nProvider locale={locale}>{children}</aria.I18nProvider>
      </aria.RouterProvider>
    </portal.PortalProvider>
  )
}
