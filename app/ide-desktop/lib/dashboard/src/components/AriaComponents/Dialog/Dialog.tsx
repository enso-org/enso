import { Dialog as AriaDialog, Modal as AriaModal, Header } from 'react-aria-components'
import { twMerge } from 'tailwind-merge'

import Dismiss from 'enso-assets/dismiss.svg'

import { Button } from '#/components/AriaComponents'
import { useStrictPortalContext } from '#/components/Portal'

import type { DialogProps, DialogType } from './types'

const MODAL_CLASSES =
  'fixed z-1 top-0 left-0 right-0 bottom-0 isolate bg-black/[15%] flex items-center justify-center text-center'
const DIALOG_CLASSES =
  'flex flex-col overflow-hidden rounded-xl bg-white text-left align-middle text-slate-700 shadow-2xl bg-clip-padding border border-black/10'

const MODAL_CLASSES_BY_TYPE = {
  modal: 'p-4',
  popover: '',
  fullscreen: 'p-4',
} satisfies Record<DialogType, string>

const DIALOG_CLASSES_BY_TYPE = {
  modal: 'w-full max-w-md min-h-[200px] h-[90vh] max-h-[90vh]',
  popover: 'rounded-lg',
  fullscreen: 'w-full h-full max-w-full max-h-full bg-clip-border',
} satisfies Record<DialogType, string>

/**
 * A dialog is an overlay shown above other content in an application.
 * Can be used to display alerts, confirmations, or other content.
 */
export function Dialog(props: DialogProps) {
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

  const root = useStrictPortalContext()

  return (
    <AriaModal
      className={twMerge(MODAL_CLASSES, [MODAL_CLASSES_BY_TYPE[type]])}
      isDismissable={isDismissible}
      onOpenChange={onOpenChange}
      isKeyboardDismissDisabled={isKeyboardDismissDisabled}
      UNSTABLE_portalContainer={root.current}
      {...props}
    >
      <AriaDialog
        className={twMerge(DIALOG_CLASSES, [DIALOG_CLASSES_BY_TYPE[type]], className)}
        {...ariaDialogProps}
      >
        {opts => (
          <>
            {typeof title === 'string' && (
              <Header className="px-4 py-3 border-b shadow sticky text-primary flex center flex-none">
                <h2 className="font-semibold leading-6 my-0 text-xl">{title}</h2>

                <Button
                  variant="icon"
                  className="ml-auto my-auto"
                  onPress={opts.close}
                  icon={Dismiss}
                />
              </Header>
            )}

            <div className="flex-1 shrink-0">
              {typeof children === 'function' ? children(opts) : children}
            </div>
          </>
        )}
      </AriaDialog>
    </AriaModal>
  )
}

export { DialogTrigger, type DialogTriggerProps } from 'react-aria-components'
