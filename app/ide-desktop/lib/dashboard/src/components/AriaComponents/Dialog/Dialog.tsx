/** @file A dialog is an overlay shown above other content in an application.
 * Can be used to display alerts, confirmations, or other content. */
import * as React from 'react'

import clsx from 'clsx'
import * as twv from 'tailwind-variants'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import * as portal from '#/components/Portal'

import type * as types from './types'
import * as variants from './variants'

const OVERLAY_STYLES = twv.tv({
  base: 'fixed inset-0 isolate flex items-center justify-center bg-black/[25%]',
  variants: {
    isEntering: { true: 'animate-in fade-in duration-200 ease-out' },
    isExiting: { true: 'animate-out fade-out duration-200 ease-in' },
  },
})

const MODAL_STYLES = twv.tv({
  base: 'fixed inset-0 flex items-center justify-center text-center p-4',
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
  },
  slots: {
    header:
      'sticky grid grid-cols-[1fr_auto_1fr] items-center border-b border-primary/10 px-3.5 py-2 text-primary',
  },
})

// ==============
// === Dialog ===
// ==============

/** A dialog is an overlay shown above other content in an application.
 * Can be used to display alerts, confirmations, or other content. */
export function Dialog(props: types.DialogProps) {
  const {
    children,
    title,
    type = 'modal',
    isDismissable = true,
    isKeyboardDismissDisabled = false,
    hideCloseButton = false,
    className,
    onOpenChange = () => {},
    modalProps = {},
    ...ariaDialogProps
  } = props
  const dialogRef = React.useRef<HTMLDivElement>(null)
  const overlayState = React.useRef<aria.OverlayTriggerState | null>(null)

  const root = portal.useStrictPortalContext()
  const shouldRenderTitle = typeof title === 'string'
  const dialogSlots = DIALOG_STYLES({ className, type })

  aria.useInteractOutside({
    ref: dialogRef,
    onInteractOutside: () => {
      if (isDismissable) {
        overlayState.current?.close()
      } else {
        const duration = 200 // 200ms

        dialogRef.current?.animate(
          [{ transform: 'scale(1)' }, { transform: 'scale(1.015)' }, { transform: 'scale(1)' }],
          { duration, iterations: 2, direction: 'alternate' }
        )
      }
    },
  })

  return (
    <aria.ModalOverlay
      className={OVERLAY_STYLES}
      isDismissable={isDismissable}
      isKeyboardDismissDisabled={isKeyboardDismissDisabled}
      UNSTABLE_portalContainer={root.current}
      onOpenChange={onOpenChange}
      {...modalProps}
    >
      {values => {
        overlayState.current = values.state

        return (
          <aria.Modal
            className={MODAL_STYLES}
            isDismissable={isDismissable}
            isKeyboardDismissDisabled={isKeyboardDismissDisabled}
            UNSTABLE_portalContainer={root.current}
            onOpenChange={onOpenChange}
            {...modalProps}
          >
            <aria.Dialog ref={dialogRef} className={dialogSlots.base()} {...ariaDialogProps}>
              {opts => (
                <>
                  {shouldRenderTitle && (
                    <aria.Header className={dialogSlots.header()}>
                      <ariaComponents.CloseButton
                        className={clsx('mr-auto mt-0.5', { hidden: hideCloseButton })}
                        onPress={opts.close}
                      />

                      <aria.Heading
                        slot="title"
                        level={2}
                        className="my-0 text-base font-semibold leading-6"
                      >
                        {title}
                      </aria.Heading>
                    </aria.Header>
                  )}

                  <div className="relative flex-auto overflow-y-auto p-3.5">
                    {typeof children === 'function' ? children(opts) : children}
                  </div>
                </>
              )}
            </aria.Dialog>
          </aria.Modal>
        )
      }}
    </aria.ModalOverlay>
  )
}
