/**
 * @file
 * The root component with required providers
 */
import * as React from 'react'

import * as reactAriaComponents from 'react-aria-components'

import * as portal from '#/components/Portal'

/**
 * Props for the root component
 */
export interface RootProps extends React.PropsWithChildren {
  readonly rootRef: React.RefObject<HTMLElement>
  readonly navigate: (path: string) => void
  readonly locale?: string
}

/**
 * The root component with required providers
 */
export function Root(props: RootProps) {
  const { children, rootRef, navigate, locale = 'en-US' } = props

  return (
    <portal.PortalProvider value={rootRef}>
      <reactAriaComponents.RouterProvider navigate={navigate}>
        <reactAriaComponents.I18nProvider locale={locale}>
          {children}
        </reactAriaComponents.I18nProvider>
      </reactAriaComponents.RouterProvider>
    </portal.PortalProvider>
  )
}
