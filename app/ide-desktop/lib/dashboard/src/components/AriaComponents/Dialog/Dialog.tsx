/** @file A dialog is an overlay shown above other content in an application.
 * Can be used to display alerts, confirmations, or other content. */
import * as React from 'react'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import * as errorBoundary from '#/components/ErrorBoundary'
import * as loader from '#/components/Loader'
import * as portal from '#/components/Portal'

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
export interface DialogProps extends types.DialogProps, twv.VariantProps<typeof DIALOG_STYLES> {}

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
  base: '',
  variants: {
    type: {
      modal: 'w-full max-w-md min-h-[100px] max-h-[90vh]',
      fullscreen: 'w-full h-full max-w-full max-h-full bg-clip-border',
    },
    hideCloseButton: { true: { closeButton: 'hidden' } },
  },
  slots: {
    header:
      'sticky grid grid-cols-[1fr_auto_1fr] items-center border-b border-primary/10 px-3.5 pt-[3px] pb-0.5',
    closeButton: 'col-start-1 col-end-1 mr-auto',
    heading: 'col-start-2 col-end-2 my-0',
    content: 'relative flex-auto overflow-y-auto p-3.5',
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
    closeButton = 'none',
    isDismissable = true,
    isKeyboardDismissDisabled = false,
    hideCloseButton = false,
    className,
    onOpenChange = () => {},
    modalProps = {},
    testId = 'dialog',
    rounded,
    ...ariaDialogProps
  } = props

  const dialogId = aria.useId()
  const dialogRef = React.useRef<HTMLDivElement>(null)
  const overlayState = React.useRef<aria.OverlayTriggerState | null>(null)
  const root = portal.useStrictPortalContext()
  const shouldRenderTitle = typeof title === 'string'
  const dialogSlots = DIALOG_STYLES({ className, type, rounded, hideCloseButton })

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
          { duration, iterations: 1, direction: 'alternate' }
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
      {values => {
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
                ref={mergeRefs.mergeRefs(dialogRef, element => {
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
                className={dialogSlots.base()}
                {...ariaDialogProps}
              >
                {opts => (
                  <dialogProvider.DialogProvider value={{ close: opts.close, dialogId }}>
                    {shouldRenderTitle && (
                      <aria.Header className={dialogSlots.header()}>
                        <ariaComponents.CloseButton
                          className={dialogSlots.closeButton()}
                          onPress={opts.close}
                        />

                        <ariaComponents.Text.Heading
                          slot="title"
                          level={2}
                          className={dialogSlots.heading()}
                        >
                          {title}
                        </ariaComponents.Text.Heading>
                      </aria.Header>
                    )}

                    <div className={dialogSlots.content()}>
                      <errorBoundary.ErrorBoundary>
                        <React.Suspense fallback={<loader.Loader minHeight="h32" />}>
                          {typeof children === 'function' ? children(opts) : children}
                        </React.Suspense>
                      </errorBoundary.ErrorBoundary>
                    </div>
                    {closeButton === 'floating' && (
                      <div className="absolute m-[19px] flex gap-1">
                        <ariaComponents.CloseButton onPress={opts.close} />
                      </div>
                    )}
                  </dialogProvider.DialogProvider>
                )}
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
