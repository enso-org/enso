/** @file A wrapper containing all UI-related React Provdiers. */
import * as React from 'react'

import { I18nProvider } from '#/components/aria'
import { DialogStackProvider } from '#/components/AriaComponents'
import { PortalProvider } from '#/components/Portal'

// ===================
// === UIProviders ===
// ===================

/** Props for a {@link UIProviders}. */
export interface UIProvidersProps extends Readonly<React.PropsWithChildren> {
  readonly portalRoot: Element
  readonly locale: string
}

/** A wrapper containing all UI-related React Provdiers. */
export default function UIProviders(props: UIProvidersProps) {
  const { portalRoot, locale, children } = props
  return (
    <PortalProvider value={portalRoot}>
      <DialogStackProvider>
        <I18nProvider locale={locale}>{children}</I18nProvider>
      </DialogStackProvider>
    </PortalProvider>
  )
}
