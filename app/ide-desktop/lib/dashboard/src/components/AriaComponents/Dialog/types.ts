/** @file Types for the Dialog component. */
import type * as aria from '#/components/aria'

/** Props for the Dialog component. */
export interface DialogProps extends aria.DialogProps {
  /** The type of dialog to render.
   * @default 'modal' */
  readonly title?: string
  readonly isDismissable?: boolean
  readonly hideCloseButton?: boolean
  readonly onOpenChange?: (isOpen: boolean) => void
  readonly isKeyboardDismissDisabled?: boolean
  readonly modalProps?: Pick<aria.ModalOverlayProps, 'className' | 'defaultOpen' | 'isOpen'>

  readonly testId?: string
}

/** The props for the DialogTrigger component. */
export interface DialogTriggerProps extends aria.DialogTriggerProps {}
