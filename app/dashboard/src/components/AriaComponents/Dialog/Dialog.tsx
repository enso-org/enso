/** @file A dialog is an overlay shown above other content in an application.
 * Can be used to display alerts, confirmations, or other content. */
import * as React from 'react'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import * as errorBoundary from '#/components/ErrorBoundary'
import * as portal from '#/components/Portal'
import * as suspense from '#/components/Suspense'

import * as mergeRefs from '#/utilities/mergeRefs'
import * as twv from '#/utilities/tailwindVariants'

import * as dialogProvider from './DialogProvider'
import * as dialogStackProvider from './DialogStackProvider'
import type * as types from './types'
import * as utlities from './utilities'
import * as variants from './variants'

// =================
// === Constants ===
// =================
/**
 * Props for the {@link Dialog} component.
 */
export interface DialogProps
  extends types.DialogProps,
    Omit<twv.VariantProps<typeof DIALOG_STYLES>, 'scrolledToTop'> {}

const OVERLAY_STYLES = twv.tv({
  base: 'fixed inset-0 isolate flex items-center justify-center bg-black/[25%]',
  variants: {
    isEntering: { true: 'animate-in fade-in duration-200 ease-out' },
    isExiting: { true: 'animate-out fade-out duration-200 ease-in' },
  },
})

const MODAL_STYLES = twv.tv({
  base: 'fixed inset-0 flex items-center justify-center text-center text-xs text-primary p-4',
  variants: {
    isEntering: { true: 'animate-in slide-in-from-top-1 ease-out duration-200' },
    isExiting: { true: 'animate-out slide-out-to-top-1 ease-in duration-200' },
  },
})

const DIALOG_STYLES = twv.tv({
  extend: variants.DIALOG_STYLES,
  base: 'w-full',
  variants: {
    type: {
      modal: {
        base: 'w-full min-h-[100px] max-h-[90vh]',
        header: 'px-3.5 pt-[3px] pb-0.5 min-h-[42px]',
      },
      fullscreen: {
        base: 'w-full h-full max-w-full max-h-full bg-clip-border',
        header: 'px-4 pt-[5px] pb-1.5 min-h-12',
      },
    },
    fitContent: {
      true: {
        base: 'min-w-max',
        content: 'min-w-max',
      },
    },
    hideCloseButton: { true: { closeButton: 'hidden' } },
    closeButton: {
      normal: { base: '', closeButton: '' },
      floating: {
        base: '',
        closeButton: 'absolute left-4 top-4 visible z-1 transition-all duration-150',
        header: 'p-0 max-h-0 min-h-0 h-0 border-0 z-1',
        content: 'isolate',
      },
    },
    /**
     * The size of the dialog.
     * Only applies to the `modal` type.
     */
    size: {
      small: { base: '' },
      medium: { base: '' },
      large: { base: '' },
      xlarge: { base: '' },
      xxlarge: { base: '' },
      xxxlarge: { base: '' },
      xxxxlarge: { base: '' },
    },
    scrolledToTop: { true: { header: 'border-transparent' } },
  },
  slots: {
    header:
      'sticky grid grid-cols-[1fr_auto_1fr] items-center border-b border-primary/10 transition-[border-color] duration-150',
    closeButton: 'col-start-1 col-end-1 mr-auto',
    heading: 'col-start-2 col-end-2 my-0 text-center',
    content: 'relative flex-auto overflow-y-auto p-3.5',
  },
  compoundVariants: [
    { type: 'modal', size: 'small', class: 'max-w-sm' },
    { type: 'modal', size: 'medium', class: 'max-w-md' },
    { type: 'modal', size: 'large', class: 'max-w-lg' },
    { type: 'modal', size: 'xlarge', class: 'max-w-xl' },
    { type: 'modal', size: 'xxlarge', class: 'max-w-2xl' },
    { type: 'modal', size: 'xxxlarge', class: 'max-w-3xl' },
    { type: 'modal', size: 'xxxxlarge', class: 'max-w-4xl' },
  ],
  defaultVariants: {
    type: 'modal',
    closeButton: 'normal',
    hideCloseButton: false,
    size: 'medium',
  },
})

// ==============
// === Dialog ===
// ==============

/** A dialog is an overlay shown above other content in an application.
 * Can be used to display alerts, confirmations, or other content. */
export function Dialog(props: DialogProps) {
  const {
    children,
    title,
    type = 'modal',
    closeButton = 'normal',
    isDismissable = true,
    isKeyboardDismissDisabled = false,
    hideCloseButton = false,
    className,
    onOpenChange = () => {},
    modalProps = {},
    testId = 'dialog',
    size,
    rounded,
    fitContent,
    ...ariaDialogProps
  } = props

  const [isScrolledToTop, setIsScrolledToTop] = React.useState(true)

  /**
   * Handles the scroll event on the dialog content.
   */
  const handleScroll = (scrollTop: number) => {
    if (scrollTop > 0) {
      setIsScrolledToTop(false)
    } else {
      setIsScrolledToTop(true)
    }
  }

  const dialogId = aria.useId()
  const dialogRef = React.useRef<HTMLDivElement>(null)
  const overlayState = React.useRef<aria.OverlayTriggerState | null>(null)
  const root = portal.useStrictPortalContext()

  const styles = DIALOG_STYLES({
    className,
    type,
    rounded,
    hideCloseButton,
    closeButton,
    scrolledToTop: isScrolledToTop,
    size,
    fitContent,
  })

  utlities.useInteractOutside({
    ref: dialogRef,
    id: dialogId,
    onInteractOutside: () => {
      if (isDismissable) {
        overlayState.current?.close()
      } else {
        const duration = 200 // 200ms
        dialogRef.current?.animate(
          [{ transform: 'scale(1)' }, { transform: 'scale(1.015)' }, { transform: 'scale(1)' }],
          { duration, iterations: 1, direction: 'alternate' },
        )
      }
    },
  })

  return (
    <aria.ModalOverlay
      className={OVERLAY_STYLES}
      isDismissable={isDismissable}
      isKeyboardDismissDisabled={isKeyboardDismissDisabled}
      UNSTABLE_portalContainer={root}
      onOpenChange={onOpenChange}
      shouldCloseOnInteractOutside={() => false}
      {...modalProps}
    >
      {(values) => {
        overlayState.current = values.state

        return (
          <aria.Modal
            className={MODAL_STYLES}
            isDismissable={isDismissable}
            isKeyboardDismissDisabled={isKeyboardDismissDisabled}
            UNSTABLE_portalContainer={root}
            onOpenChange={onOpenChange}
            shouldCloseOnInteractOutside={() => false}
            {...modalProps}
          >
            <dialogStackProvider.DialogStackRegistrar
              id={dialogId}
              type={TYPE_TO_DIALOG_TYPE[type]}
            >
              <aria.Dialog
                id={dialogId}
                ref={mergeRefs.mergeRefs(dialogRef, (element) => {
                  if (element) {
                    // This is a workaround for the `data-testid` attribute not being
                    // supported by the 'react-aria-components' library.
                    // We need to set the `data-testid` attribute on the dialog element
                    // so that we can use it in our tests.
                    // This is a temporary solution until we refactor the Dialog component
                    // to use `useDialog` hook from the 'react-aria-components' library.
                    // this will allow us to set the `data-testid` attribute on the dialog
                    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
                    element.dataset.testId = testId
                  }
                })}
                className={styles.base()}
                {...ariaDialogProps}
              >
                {(opts) => {
                  return (
                    <dialogProvider.DialogProvider value={{ close: opts.close, dialogId }}>
                      <aria.Header className={styles.header({ scrolledToTop: isScrolledToTop })}>
                        <ariaComponents.CloseButton
                          className={styles.closeButton()}
                          onPress={opts.close}
                        />

                        {title != null && (
                          <ariaComponents.Text.Heading
                            slot="title"
                            level={2}
                            className={styles.heading()}
                            weight="semibold"
                          >
                            {title}
                          </ariaComponents.Text.Heading>
                        )}
                      </aria.Header>

                      <div
                        ref={(ref) => {
                          if (ref) {
                            handleScroll(ref.scrollTop)
                          }
                        }}
                        className={styles.content()}
                        onScroll={(event) => {
                          handleScroll(event.currentTarget.scrollTop)
                        }}
                      >
                        <errorBoundary.ErrorBoundary>
                          <suspense.Suspense
                            loaderProps={{ minHeight: type === 'fullscreen' ? 'full' : 'h32' }}
                          >
                            {typeof children === 'function' ? children(opts) : children}
                          </suspense.Suspense>
                        </errorBoundary.ErrorBoundary>
                      </div>
                    </dialogProvider.DialogProvider>
                  )
                }}
              </aria.Dialog>
            </dialogStackProvider.DialogStackRegistrar>
          </aria.Modal>
        )
      }}
    </aria.ModalOverlay>
  )
}

const TYPE_TO_DIALOG_TYPE: Record<
  NonNullable<DialogProps['type']>,
  dialogStackProvider.DialogStackItem['type']
> = {
  modal: 'dialog',
  fullscreen: 'dialog-fullscreen',
}
