/**
 * @file
 * A dialog is an overlay shown above other content in an application.
 * Can be used to display alerts, confirmations, or other content.
 */
import * as reactAriaComponents from 'react-aria-components'
import * as tailwindMerge from 'tailwind-merge'

import Dismiss from 'enso-assets/dismiss.svg'

import * as ariaComponents from '#/components/AriaComponents'
import * as portal from '#/components/Portal'

import type * as types from './types'

const MODAL_CLASSES =
  'fixed z-1 top-0 left-0 right-0 bottom-0 isolate bg-black/[15%] flex items-center justify-center text-center'
const DIALOG_CLASSES =
  'flex flex-col overflow-hidden rounded-xl bg-white text-left align-middle text-slate-700 shadow-2xl bg-clip-padding border border-black/10'

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
    isDismissible = true,
    onOpenChange = () => {},
    isKeyboardDismissDisabled = false,
    className,

    ...ariaDialogProps
  } = props

  const root = portal.useStrictPortalContext()

  return (
    <reactAriaComponents.Modal
      className={tailwindMerge.twMerge(MODAL_CLASSES, [MODAL_CLASSES_BY_TYPE[type]])}
      isDismissable={isDismissible}
      onOpenChange={onOpenChange}
      isKeyboardDismissDisabled={isKeyboardDismissDisabled}
      UNSTABLE_portalContainer={root.current}
      {...props}
    >
      <reactAriaComponents.Dialog
        className={tailwindMerge.twMerge(DIALOG_CLASSES, [DIALOG_CLASSES_BY_TYPE[type]], className)}
        {...ariaDialogProps}
      >
        {opts => (
          <>
            {typeof title === 'string' && (
              <reactAriaComponents.Header className="center sticky flex flex-none border-b px-4 py-3 text-primary shadow">
                <h2 className="my-0 text-xl font-semibold leading-6">{title}</h2>

                <ariaComponents.Button
                  variant="icon"
                  className="my-auto ml-auto"
                  onPress={opts.close}
                  icon={Dismiss}
                />
              </reactAriaComponents.Header>
            )}

            <div className="flex-1 shrink-0">
              {typeof children === 'function' ? children(opts) : children}
            </div>
          </>
        )}
      </reactAriaComponents.Dialog>
    </reactAriaComponents.Modal>
  )
}

// eslint-disable-next-line no-restricted-syntax
export { DialogTrigger, type DialogTriggerProps } from 'react-aria-components'