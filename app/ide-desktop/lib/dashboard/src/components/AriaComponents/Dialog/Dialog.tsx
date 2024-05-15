/** @file A dialog is an overlay shown above other content in an application.
 * Can be used to display alerts, confirmations, or other content. */
import * as React from 'react'

import clsx from 'clsx'
import * as tailwindMerge from 'tailwind-merge'

import Dismiss from 'enso-assets/dismiss.svg'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import * as portal from '#/components/Portal'
import WindowButton from '#/components/styled/WindowButton'

import type * as types from './types'

// =================
// === Constants ===
// =================

const MODAL_CLASSES = 'fixed z-1 inset bg-dim flex items-center justify-center text-center'
const DIALOG_CLASSES =
  'relative flex flex-col overflow-hidden text-xs rounded-default text-left align-middle text-primary before:absolute before:inset before:rounded-default before:bg-selected-frame before:backdrop-blur-default'

const MODAL_CLASSES_BY_TYPE: Readonly<Record<types.DialogType, string>> = {
  modal: '',
  popover: '',
  fullscreen: 'p-4',
}

const DIALOG_CLASSES_BY_TYPE: Readonly<Record<types.DialogType, string>> = {
  modal: 'w-full max-w-md min-h-[100px] max-h-[90vh]',
  popover: 'rounded-lg',
  fullscreen: 'w-full h-full max-w-full max-h-full bg-clip-border',
}

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
    closeButton = 'none',
    isDismissable = true,
    isKeyboardDismissDisabled = false,
    hideCloseButton = false,
    className,
    onOpenChange = () => {},
    modalProps,
    ...ariaDialogProps
  } = props
  const cleanupRef = React.useRef(() => {})

  const root = portal.useStrictPortalContext()

  return (
    <aria.Modal
      className={tailwindMerge.twMerge(MODAL_CLASSES, MODAL_CLASSES_BY_TYPE[type])}
      isDismissable={isDismissable}
      isKeyboardDismissDisabled={isKeyboardDismissDisabled}
      UNSTABLE_portalContainer={root.current}
      onOpenChange={onOpenChange}
      {...modalProps}
    >
      <aria.Dialog
        className={tailwindMerge.twMerge(DIALOG_CLASSES, DIALOG_CLASSES_BY_TYPE[type], className)}
        {...ariaDialogProps}
        ref={element => {
          cleanupRef.current()
          if (element == null) {
            cleanupRef.current = () => {}
          } else {
            const onClick = (event: Event) => {
              event.stopPropagation()
            }
            element.addEventListener('click', onClick)
            cleanupRef.current = () => {
              element.removeEventListener('click', onClick)
            }
          }
        }}
      >
        {opts => (
          <>
            {typeof title === 'string' && (
              <aria.Header className="center sticky flex flex-none items-center border-b px-3.5 py-2.5 text-primary shadow">
                <aria.Heading
                  slot="title"
                  level={2}
                  className="text-l my-0 font-semibold leading-6"
                >
                  {title}
                </aria.Heading>

                <ariaComponents.Button
                  variant="icon"
                  className={clsx('my-auto ml-auto mr-[-4px]', { hidden: hideCloseButton })}
                  size="custom"
                  onPress={opts.close}
                  icon={Dismiss}
                />
              </aria.Header>
            )}
            <div className="flex-1 shrink-0 overflow-auto">
              {typeof children === 'function' ? children(opts) : children}
            </div>
            {closeButton === 'floating' && (
              <div className="absolute m-floating-buttons flex gap-1">
                <WindowButton role="close" onPress={opts.close} />
              </div>
            )}
          </>
        )}
      </aria.Dialog>
    </aria.Modal>
  )
}
