/**
 * @file
 * A dialog is an overlay shown above other content in an application.
 * Can be used to display alerts, confirmations, or other content.
 */
import * as aria from '#/components/aria'
import { DialogTrigger } from '#/components/Dialog/DialogTrigger'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { usePortalContext } from '#/components/Portal'
import * as suspense from '#/components/Suspense'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import * as React from 'react'
import { ResetButtonGroupContext } from '../Button'
import type { Placement } from '../types'
import { Close } from './Close'
import { DialogProvider } from './DialogProvider'
import { DialogStackRegistrar } from './DialogStackProvider'
import { useInteractOutside } from './utilities'
import { DIALOG_BACKGROUND } from './variants'

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

// eslint-disable-next-line react-refresh/only-export-components
export const POPOVER_STYLES = tv({
  base: 'shadow-xl w-full overflow-clip',
  variants: {
    variant: {
      custom: { dialog: '' },
      light: { base: DIALOG_BACKGROUND({ variant: 'light' }) },
      dark: { base: DIALOG_BACKGROUND({ variant: 'dark' }) },
    },
    isEntering: {
      true: 'animate-in fade-in placement-bottom:slide-in-from-top-1 placement-top:slide-in-from-bottom-1 placement-left:slide-in-from-right-1 placement-right:slide-in-from-left-1 ease-out duration-200',
    },
    isExiting: {
      true: 'animate-out fade-out placement-bottom:slide-out-to-top-1 placement-top:slide-out-to-bottom-1 placement-left:slide-out-to-right-1 placement-right:slide-out-to-left-1 ease-in duration-150',
    },
    size: {
      custom: { base: '', dialog: '' },
      /* eslint-disable @typescript-eslint/naming-convention */
      'auto-xxsmall': { base: 'w-[unset]', dialog: 'p-1.5' },
      /* eslint-enable @typescript-eslint/naming-convention */
      auto: { base: 'w-[unset]', dialog: 'p-2.5' },
      xxsmall: { base: 'max-w-[206px]', dialog: 'p-1.5' },
      xsmall: { base: 'max-w-xs', dialog: 'p-3' },
      small: { base: 'max-w-sm', dialog: 'px-4 p-3' },
      medium: { base: 'max-w-md', dialog: 'px-5 p-3.5' },
      large: { base: 'max-w-lg', dialog: 'p-4' },
      xlarge: { base: 'max-w-xl', dialog: 'p-6' },
      xxlarge: { base: 'max-w-2xl', dialog: 'px-8 py-7' },
      xxxlarge: { base: 'max-w-3xl', dialog: 'px-10 py-9' },
    },
    rounded: {
      none: { base: 'rounded-none', dialog: 'rounded-none' },
      small: { base: 'rounded-sm', dialog: 'rounded-sm scroll-offset-edge-md' },
      medium: { base: 'rounded-md', dialog: 'rounded-md scroll-offset-edge-xl' },
      large: { base: 'rounded-lg', dialog: 'rounded-lg scroll-offset-edge-xl' },
      xlarge: { base: 'rounded-xl', dialog: 'rounded-xl scroll-offset-edge-xl' },
      xxlarge: { base: 'rounded-2xl', dialog: 'rounded-2xl scroll-offset-edge-2xl' },
      xxxlarge: { base: 'rounded-3xl', dialog: 'rounded-3xl scroll-offset-edge-3xl' },
      xxxxlarge: { base: 'rounded-4xl', dialog: 'rounded-4xl scroll-offset-edge-4xl' },
    },
  },
  slots: {
    dialog: 'flex-auto overflow-y-auto max-h-[inherit]',
  },
  defaultVariants: { rounded: 'xxlarge', size: 'small', variant: 'light' },
})

const SUSPENSE_LOADER_PROPS = { minHeight: 'h32' } as const

/**
 * A popover is an overlay element positioned relative to a trigger.
 * It can be used to display additional content or actions.
 */
export const Popover = Object.assign(
  React.forwardRef(function Popover(props: PopoverProps, ref: React.ForwardedRef<HTMLElement>) {
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

    const root = usePortalContext()
    const popoverStyle = { zIndex: '' }

    return (
      <aria.Popover
        ref={ref}
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
        {...(placement != null ? { placement } : {})}
        {...ariaPopoverProps}
      >
        {(opts) => (
          <PopoverContent
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
  }),
  {
    // eslint-disable-next-line @typescript-eslint/naming-convention
    Trigger: DialogTrigger,
    // eslint-disable-next-line @typescript-eslint/naming-convention
    Close,
  },
)

/**
 * Props for a {@link PopoverContent}.
 */
interface PopoverContentProps {
  readonly children: PopoverProps['children']
  readonly size: PopoverProps['size']
  readonly rounded: PopoverProps['rounded']
  readonly opts: aria.PopoverRenderProps
  readonly isDismissable: boolean
  readonly variant: PopoverProps['variant']
  readonly onClose?: (() => void) | undefined
}

/**
 * The content of a popover.
 */
function PopoverContent(props: PopoverContentProps) {
  const { children, size, rounded, opts, isDismissable, variant, onClose } = props

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

  useInteractOutside({
    ref: dialogRef,
    id: dialogId,
    onInteractOutside: (e) => {
      // Do not close dialog if another dialog was clicked.
      // This can happen in e.g. a `ComboBox` nested in a `Popover`.
      if (
        e.target instanceof HTMLElement &&
        e.target.dataset.testid !== 'underlay' &&
        document.getElementById('enso-portal-root')?.contains(e.target) === true
      ) {
        return
      }
      if (isDismissable) {
        close()
      }
    },
  })

  return (
    <ResetButtonGroupContext>
      <DialogStackRegistrar id={dialogId} type="popover" />
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
        <DialogProvider dialogId={dialogId} close={close}>
          <ErrorBoundary>
            <suspense.Suspense loaderProps={SUSPENSE_LOADER_PROPS}>
              {typeof children === 'function' ? children({ ...opts, close }) : children}
            </suspense.Suspense>
          </ErrorBoundary>
        </DialogProvider>
      </div>
    </ResetButtonGroupContext>
  )
}
