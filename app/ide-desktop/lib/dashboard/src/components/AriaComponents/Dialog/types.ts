/** @file Types for the Dialog component. */
import type * as aria from '#/components/aria'

/** The type of Dialog. */
export type DialogType = 'fullscreen' | 'modal' | 'popover'

/** The type of close button for the Dialog.
 * Note that Dialogs with a title have a regular close button by default. */
export type DialogCloseButtonType = 'floating' | 'none'

/** Props for the Dialog component. */
export interface DialogProps extends aria.DialogProps {
  /** The type of dialog to render.
   * @default 'modal' */
  readonly type?: DialogType
  readonly closeButton?: DialogCloseButtonType
  readonly title?: string
  readonly isDismissable?: boolean
  readonly hideCloseButton?: boolean
  readonly onOpenChange?: (isOpen: boolean) => void
  readonly isKeyboardDismissDisabled?: boolean
  readonly modalProps?: Pick<aria.ModalOverlayProps, 'className' | 'defaultOpen' | 'isOpen'>
}

/** The props for the DialogTrigger component. */
export interface DialogTriggerProps extends aria.DialogTriggerProps {}
