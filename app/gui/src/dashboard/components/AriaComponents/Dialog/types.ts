/** @file Types for the Dialog component. */
import type * as aria from '#/components/aria'

/** Props for the Dialog component. */
export interface DialogProps extends aria.DialogProps {
  readonly title?: string
  readonly isDismissable?: boolean
  readonly onOpenChange?: (isOpen: boolean) => void
  readonly isKeyboardDismissDisabled?: boolean
  readonly modalProps?: Pick<aria.ModalOverlayProps, 'className' | 'defaultOpen' | 'isOpen'>
  readonly testId?: string
}
