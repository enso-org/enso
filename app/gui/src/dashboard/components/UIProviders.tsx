/** @file A wrapper containing all UI-related React Provdiers. */
import * as React from 'react'

import { DialogStackProvider } from '#/components/AriaComponents'
import { PortalProvider } from '#/components/Portal'
import type { Spring } from 'framer-motion'
import { MotionConfig } from 'framer-motion'
import { I18nProvider } from 'react-aria-components'

const RootContext = React.createContext<{
  app: HTMLElement
  portal: HTMLElement
  // This is safe, because the default value is never used
  // eslint-disable-next-line no-restricted-syntax
}>({
  app: null,
  portal: null,
} as unknown as {
  app: HTMLElement
  portal: HTMLElement
})

const DEFAULT_TRANSITION_OPTIONS: Spring = {
  type: 'spring',
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  stiffness: 200,
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  damping: 30,
  mass: 1,
  velocity: 0,
}

/** Props for a {@link UIProviders}. */
export interface UIProvidersProps extends Readonly<React.PropsWithChildren> {
  readonly appRoot: HTMLElement
  readonly portalRoot: HTMLElement
  readonly locale: string
}

/** A wrapper containing all UI-related React Provdiers. */
export default function UIProviders(props: UIProvidersProps) {
  const { appRoot, portalRoot, locale, children } = props

  return (
    <RootContext.Provider value={{ app: appRoot, portal: portalRoot }}>
      <MotionConfig reducedMotion="user" transition={DEFAULT_TRANSITION_OPTIONS}>
        <PortalProvider value={portalRoot}>
          <DialogStackProvider>
            <I18nProvider locale={locale}>{children}</I18nProvider>
          </DialogStackProvider>
        </PortalProvider>
      </MotionConfig>
    </RootContext.Provider>
  )
}

/**
 * A hook that returns the root context.
 * @returns The root context.
 */
export function useRootContext() {
  return React.useContext(RootContext)
}
