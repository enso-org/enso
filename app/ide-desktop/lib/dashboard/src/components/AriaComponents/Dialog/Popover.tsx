/**
 * @file
 * A dialog is an overlay shown above other content in an application.
 * Can be used to display alerts, confirmations, or other content.
 */
import * as React from 'react'

import * as twv from 'tailwind-variants'

import * as aria from '#/components/aria'
import * as errorBoundary from '#/components/ErrorBoundary'
import * as loader from '#/components/Loader'
import * as portal from '#/components/Portal'

import * as dialogProvider from './DialogProvider'
import * as dialogStackProvider from './DialogStackProvider'
import * as utlities from './utilities'
import * as variants from './variants'

/**
 * Props for the Popover component.
 */
export interface PopoverProps
  extends Omit<aria.PopoverProps, 'children'>,
    twv.VariantProps<typeof POPOVER_STYLES> {
  readonly children:
    | React.ReactNode
    // eslint-disable-next-line no-restricted-syntax
    | ((opts: aria.PopoverRenderProps & { readonly close: () => void }) => React.ReactNode)
}

export const POPOVER_STYLES = twv.tv({
  extend: variants.DIALOG_BACKGROUND,
  base: 'shadow-2xl w-full',
  variants: {
    isEntering: {
      true: 'animate-in fade-in placement-bottom:slide-in-from-top-1 placement-top:slide-in-from-bottom-1 placement-left:slide-in-from-right-1 placement-right:slide-in-from-left-1 ease-out duration-200',
    },
    isExiting: {
      true: 'animate-out fade-out placement-bottom:slide-out-to-top-1 placement-top:slide-out-to-bottom-1 placement-left:slide-out-to-right-1 placement-right:slide-out-to-left-1 ease-in duration-150',
    },
    size: {
      xsmall: { base: 'max-w-xs', content: 'p-2.5' },
      small: { base: 'max-w-sm', content: 'p-3.5' },
      medium: { base: 'max-w-md', content: 'p-3.5' },
      large: { base: 'max-w-lg', content: 'px-4 py-4' },
      hero: { base: 'max-w-xl', content: 'px-6 py-5' },
    },
    rounded: {
      none: '',
      small: 'rounded-sm before:rounded-sm',
      medium: 'rounded-md before:rounded-md',
      large: 'rounded-lg before:rounded-lg',
      xlarge: 'rounded-xl before:rounded-xl',
      xxlarge: 'rounded-2xl before:rounded-2xl',
      xxxlarge: 'rounded-3xl before:rounded-3xl',
    },
  },
  slots: {
    content: 'flex-auto overflow-y-auto',
  },
  defaultVariants: { rounded: 'xxlarge', size: 'small' },
})

/**
 * A popover is an overlay element positioned relative to a trigger.
 * It can be used to display additional content or actions.*
 */
export function Popover(props: PopoverProps) {
  const {
    children,
    className,
    size,
    rounded,
    placement = 'bottom start',
    ...ariaPopoverProps
  } = props

  const dialogRef = React.useRef<HTMLDivElement>(null)
  const closeRef = React.useRef<(() => void) | null>(null)

  const root = portal.useStrictPortalContext()
  const dialogId = aria.useId()

  utlities.useInteractOutside({
    ref: dialogRef,
    id: dialogId,
    onInteractOutside: closeRef.current,
  })

  return (
    <aria.Popover
      className={values =>
        POPOVER_STYLES({
          ...values,
          size,
          rounded,
          className: typeof className === 'function' ? className(values) : className,
        }).base()
      }
      UNSTABLE_portalContainer={root}
      placement={placement}
      style={{ zIndex: 'unset' }}
      shouldCloseOnInteractOutside={() => false}
      {...ariaPopoverProps}
    >
      {opts => (
        <dialogStackProvider.DialogStackRegistrar id={dialogId} type="popover">
          <aria.Dialog id={dialogId} ref={dialogRef}>
            {({ close }) => {
              closeRef.current = close

              return (
                <div className={POPOVER_STYLES({ ...opts, size, rounded }).content()}>
                  <dialogProvider.DialogProvider value={{ close, dialogId }}>
                    <errorBoundary.ErrorBoundary>
                      <React.Suspense fallback={<loader.Loader minHeight="h16" />}>
                        {typeof children === 'function' ? children({ ...opts, close }) : children}
                      </React.Suspense>
                    </errorBoundary.ErrorBoundary>
                  </dialogProvider.DialogProvider>
                </div>
              )
            }}
          </aria.Dialog>
        </dialogStackProvider.DialogStackRegistrar>
      )}
    </aria.Popover>
  )
}
