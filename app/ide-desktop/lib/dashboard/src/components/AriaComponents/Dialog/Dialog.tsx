/**
 * @file
 * A dialog is an overlay shown above other content in an application.
 * Can be used to display alerts, confirmations, or other content.
 */
import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import Dismiss from 'enso-assets/dismiss.svg'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import * as portal from '#/components/Portal'
import WindowButton from '#/components/styled/WindowButton'

import type * as types from './types'

const MODAL_CLASSES =
  'fixed z-1 top-0 left-0 right-0 bottom-0 bg-black/[15%] flex items-center justify-center text-center'
const DIALOG_CLASSES =
  'relative flex flex-col overflow-hidden rounded-xl text-left align-middle text-slate-700 shadow-2xl bg-clip-padding border border-black/10 before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default'

const MODAL_CLASSES_BY_TYPE = {
  modal: 'p-4',
  popover: '',
  fullscreen: 'p-4',
} satisfies Record<types.DialogType, string>

const DIALOG_CLASSES_BY_TYPE = {
  modal: 'w-full max-w-md min-h-[200px] h-[90vh] max-h-[90vh]',
  popover: 'rounded-lg',
  fullscreen: 'w-full h-full max-w-full max-h-full bg-clip-border',
} satisfies Record<types.DialogType, string>

/**
 * A dialog is an overlay shown above other content in an application.
 * Can be used to display alerts, confirmations, or other content.
 */
export function Dialog(props: types.DialogProps) {
  const {
    children,
    title,
    type = 'modal',
    closeButton = 'none',
    isDismissible = true,
    isKeyboardDismissDisabled = false,
    className,
    ...ariaDialogProps
  } = props

  const root = portal.useStrictPortalContext()

  return (
    <aria.Modal
      className={tailwindMerge.twMerge(MODAL_CLASSES, [MODAL_CLASSES_BY_TYPE[type]])}
      isDismissable={isDismissible}
      isKeyboardDismissDisabled={isKeyboardDismissDisabled}
      UNSTABLE_portalContainer={root.current}
    >
      <aria.Dialog
        className={tailwindMerge.twMerge(DIALOG_CLASSES, [DIALOG_CLASSES_BY_TYPE[type]], className)}
        {...ariaDialogProps}
      >
        {opts => (
          <>
            {typeof title === 'string' && (
              <aria.Header className="center sticky flex flex-none border-b px-3.5 py-2.5 text-primary shadow">
                <aria.Heading level={2} className="text-l my-0 font-semibold leading-6">
                  {title}
                </aria.Heading>

                <ariaComponents.Button
                  variant="icon"
                  className="my-auto ml-auto"
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
