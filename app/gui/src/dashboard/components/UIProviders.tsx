/** @file A wrapper containing all UI-related React Provdiers. */
import { DialogStackProvider } from '#/components/Dialog'
import { PortalProvider } from '#/components/Portal'
import * as React from 'react'
import { I18nProvider } from 'react-aria-components'

const RootContext = React.createContext<HTMLElement>(document.body)

/** Props for a {@link UIProviders}. */
export interface UIProvidersProps extends Readonly<React.PropsWithChildren> {
  readonly portalRoot: HTMLElement
  readonly appRoot: HTMLElement
  readonly locale: string
}

/** A wrapper containing all UI-related React Provdiers. */
export default function UIProviders(props: UIProvidersProps) {
  const { portalRoot, appRoot, locale, children } = props

  return (
    <RootContext.Provider value={appRoot}>
      <PortalProvider value={portalRoot}>
        <DialogStackProvider>
          <I18nProvider locale={locale}>{children}</I18nProvider>
        </DialogStackProvider>
      </PortalProvider>
    </RootContext.Provider>
  )
}

/** A hook to get the root element for the application. */
// eslint-disable-next-line react-refresh/only-export-components
export function useAppRoot() {
  return React.useContext(RootContext)
}
