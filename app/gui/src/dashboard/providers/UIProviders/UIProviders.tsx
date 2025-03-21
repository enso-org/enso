/** @file A wrapper containing all UI-related React Provdiers. */
import { DialogStackProvider } from '#/components/AriaComponents/Dialog/DialogStackProvider'
import { PortalProvider } from '#/components/Portal'
import { RootContext } from '#/providers/UIProviders/constants'
import type { Spring } from 'framer-motion'
import { MotionConfig } from 'framer-motion'
import type { PropsWithChildren } from 'react'
import { I18nProvider } from 'react-aria-components'

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
export interface UIProvidersProps extends Readonly<PropsWithChildren> {
  readonly portalRoot: HTMLElement
  readonly appRoot: HTMLElement
  readonly locale: string
}

/** A wrapper containing all UI-related React Provdiers. */
export function UIProviders(props: UIProvidersProps) {
  const { portalRoot, appRoot, locale, children } = props

  return (
    <RootContext.Provider value={{ portalRoot, appRoot }}>
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
