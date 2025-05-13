/**
 * @file
 * A dialog is an overlay shown above other content in an application.
 * Can be used to display alerts, confirmations, or other content.
 */
import * as aria from '#/components/aria'
import * as errorBoundary from '#/components/ErrorBoundary'
import * as portal from '#/components/Portal'
import * as suspense from '#/components/Suspense'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type { VariantProps } from '#/utilities/tailwindVariants'
import * as React from 'react'
import { ResetButtonGroupContext } from '../Button'
import type { Placement } from '../types'
import { Close } from './Close'
import * as dialogProvider from './DialogProvider'
import * as dialogStackProvider from './DialogStackProvider'
import { DialogTrigger } from './DialogTrigger'
import * as utlities from './utilities'
import { POPOVER_STYLES } from './variants'

/** Props for a {@link Popover}. */
export interface PopoverProps
  extends Omit<aria.PopoverProps, 'children' | 'placement'>,
    VariantProps<typeof POPOVER_STYLES> {
  readonly children:
    | React.ReactNode
    | ((opts: aria.PopoverRenderProps & { readonly close: () => void }) => React.ReactNode)
  readonly isDismissable?: boolean
  readonly placement?: Placement | undefined
  readonly onClose?: (() => void) | undefined
}

const SUSPENSE_LOADER_PROPS = { minHeight: 'h32' } as const

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
    variant,
    placement,
    isDismissable = true,
    onClose,
    ...ariaPopoverProps
  } = props

  const popoverRef = React.useRef<HTMLDivElement>(null)
  const root = portal.useStrictPortalContext()
  const popoverStyle = { zIndex: '' }

  return (
    // @ts-expect-error placement is optional, but destructure it to make it either value or undefined, and ts complains
    <aria.Popover
      ref={popoverRef}
      className={(values) =>
        POPOVER_STYLES({
          isEntering: values.isEntering,
          isExiting: values.isExiting,
          size,
          rounded,
          variant,
        }).base({
          className: typeof className === 'function' ? className(values) : className,
        })
      }
      UNSTABLE_portalContainer={root}
      style={popoverStyle}
      shouldCloseOnInteractOutside={() => false}
      placement={placement}
      {...ariaPopoverProps}
    >
      {(opts) => (
        <PopoverContent
          popoverRef={popoverRef}
          size={size}
          rounded={rounded}
          opts={opts}
          isDismissable={isDismissable}
          variant={variant}
          onClose={onClose}
        >
          {children}
        </PopoverContent>
      )}
    </aria.Popover>
  )
}

/**
 * Props for a {@link PopoverContent}.
 */
interface PopoverContentProps {
  readonly children: PopoverProps['children']
  readonly size: PopoverProps['size']
  readonly rounded: PopoverProps['rounded']
  readonly opts: aria.PopoverRenderProps
  readonly popoverRef: React.RefObject<HTMLDivElement>
  readonly isDismissable: boolean
  readonly variant: PopoverProps['variant']
  readonly onClose?: (() => void) | undefined
}

/**
 * The content of a popover.
 */
function PopoverContent(props: PopoverContentProps) {
  const { children, size, rounded, opts, isDismissable, popoverRef, variant, onClose } = props

  const dialogRef = React.useRef<HTMLDivElement>(null)
  const dialogId = aria.useId()

  const contextState = React.useContext(aria.OverlayTriggerStateContext)
  const dialogContext = React.useContext(aria.DialogContext)

  // This is safe, because the labelledBy provided by DialogTrigger is always
  // passed to the DialogContext, and we check for undefined below.
  // eslint-disable-next-line no-restricted-syntax
  const labelledBy = (dialogContext as aria.DialogProps | undefined)?.['aria-labelledby']

  const close = useEventCallback(() => {
    contextState?.close()
    onClose?.()
  })

  utlities.useInteractOutside({
    ref: dialogRef,
    id: dialogId,
    onInteractOutside: useEventCallback(() => {
      if (isDismissable) {
        close()
      } else {
        if (popoverRef.current) {
          // eslint-disable-next-line @typescript-eslint/no-magic-numbers
          utlities.animateScale(popoverRef.current, 1.025)
        }
      }
    }),
  })

  return (
    <ResetButtonGroupContext>
      <dialogStackProvider.DialogStackRegistrar id={dialogId} type="popover" />
      <div
        id={dialogId}
        ref={dialogRef}
        role="dialog"
        aria-labelledby={labelledBy}
        tabIndex={-1}
        className={POPOVER_STYLES({
          ...opts,
          size,
          rounded,
          variant,
        }).dialog()}
      >
        <dialogProvider.DialogProvider dialogId={dialogId} close={close}>
          <errorBoundary.ErrorBoundary>
            <suspense.Suspense loaderProps={SUSPENSE_LOADER_PROPS}>
              {typeof children === 'function' ? children({ ...opts, close }) : children}
            </suspense.Suspense>
          </errorBoundary.ErrorBoundary>
        </dialogProvider.DialogProvider>
      </div>
    </ResetButtonGroupContext>
  )
}

Popover.Trigger = DialogTrigger
Popover.Close = Close
