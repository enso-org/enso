/**
 * @file
 * A dialog is an overlay shown above other content in an application.
 * Can be used to display alerts, confirmations, or other content.
 */
import * as React from 'react'

import * as aria from '#/components/aria'
import * as errorBoundary from '#/components/ErrorBoundary'
import * as portal from '#/components/Portal'
import * as suspense from '#/components/Suspense'

import * as twv from '#/utilities/tailwindVariants'

import * as dialogProvider from './DialogProvider'
import * as dialogStackProvider from './DialogStackProvider'
import * as utlities from './utilities'
import * as variants from './variants'

/** Props for a {@link Popover}. */
export interface PopoverProps
  extends Omit<aria.PopoverProps, 'children'>,
    twv.VariantProps<typeof POPOVER_STYLES> {
  readonly children:
    | React.ReactNode
    | ((opts: aria.PopoverRenderProps & { readonly close: () => void }) => React.ReactNode)
}

export const POPOVER_STYLES = twv.tv({
  base: 'shadow-xl w-full overflow-clip z-tooltip',
  variants: {
    isEntering: {
      true: 'animate-in fade-in placement-bottom:slide-in-from-top-1 placement-top:slide-in-from-bottom-1 placement-left:slide-in-from-right-1 placement-right:slide-in-from-left-1 ease-out duration-200',
    },
    isExiting: {
      true: 'animate-out fade-out placement-bottom:slide-out-to-top-1 placement-top:slide-out-to-bottom-1 placement-left:slide-out-to-right-1 placement-right:slide-out-to-left-1 ease-in duration-150',
    },
    size: {
      auto: { base: 'w-[unset]', dialog: 'p-2.5' },
      xxsmall: { base: 'max-w-[206px]', dialog: 'p-2' },
      xsmall: { base: 'max-w-xs', dialog: 'p-2.5' },
      small: { base: 'max-w-sm', dialog: 'p-3.5' },
      medium: { base: 'max-w-md', dialog: 'p-3.5' },
      large: { base: 'max-w-lg', dialog: 'px-4 py-4' },
      hero: { base: 'max-w-xl', dialog: 'px-6 py-5' },
    },
    rounded: {
      none: { base: 'rounded-none', dialog: 'rounded-none' },
      small: { base: 'rounded-sm', dialog: 'rounded-sm' },
      medium: { base: 'rounded-md', dialog: 'rounded-md' },
      large: { base: 'rounded-lg', dialog: 'rounded-lg' },
      xlarge: { base: 'rounded-xl', dialog: 'rounded-xl' },
      xxlarge: { base: 'rounded-2xl', dialog: 'rounded-2xl' },
      xxxlarge: { base: 'rounded-3xl', dialog: 'rounded-3xl' },
      xxxxlarge: { base: 'rounded-4xl', dialog: 'rounded-4xl' },
    },
  },
  slots: {
    dialog: variants.DIALOG_BACKGROUND({
      class: 'flex-auto overflow-y-auto max-h-[inherit]',
    }),
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
      className={(values) =>
        POPOVER_STYLES({
          ...values,
          size,
          rounded,
          className: typeof className === 'function' ? className(values) : className,
        }).base()
      }
      UNSTABLE_portalContainer={root}
      placement={placement}
      style={{
        // Unset the default z-index set by react-aria-components.
        zIndex: '',
      }}
      shouldCloseOnInteractOutside={() => false}
      {...ariaPopoverProps}
    >
      {(opts) => (
        <dialogStackProvider.DialogStackRegistrar id={dialogId} type="popover">
          <aria.Dialog
            id={dialogId}
            ref={dialogRef}
            className={POPOVER_STYLES({ ...opts, size, rounded }).dialog()}
          >
            {({ close }) => {
              closeRef.current = close

              return (
                <dialogProvider.DialogProvider value={{ close, dialogId }}>
                  <errorBoundary.ErrorBoundary>
                    <suspense.Suspense loaderProps={{ minHeight: 'h32' }}>
                      {typeof children === 'function' ? children({ ...opts, close }) : children}
                    </suspense.Suspense>
                  </errorBoundary.ErrorBoundary>
                </dialogProvider.DialogProvider>
              )
            }}
          </aria.Dialog>
        </dialogStackProvider.DialogStackRegistrar>
      )}
    </aria.Popover>
  )
}
