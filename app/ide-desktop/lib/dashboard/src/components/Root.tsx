import * as React from 'react'

import { I18nProvider, RouterProvider } from 'react-aria-components'

import { PortalProvider } from '#/components/Portal'

/**
 *
 */
export interface RootProps extends React.PropsWithChildren {
  rootRef: React.RefObject<HTMLElement>
  navigate: (path: string) => void
  locale?: string
}

/**
 * The root component with required providers
 */
export function Root(props: RootProps) {
  const { children, rootRef, navigate, locale = 'en-US' } = props

  return (
    <PortalProvider value={rootRef}>
      <RouterProvider navigate={navigate}>
        <I18nProvider locale={locale}>{children}</I18nProvider>
      </RouterProvider>
    </PortalProvider>
  )
}
