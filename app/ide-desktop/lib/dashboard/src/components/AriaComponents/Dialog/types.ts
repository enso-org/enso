import type { DialogProps as AriaDialogProps } from 'react-aria-components'

/**
 *
 */
export type DialogType = 'fullscreen' | 'modal' | 'popover'

/**
 *
 */
export interface DialogProps extends AriaDialogProps {
  /**
   * The type of dialog to render.
   * @default 'modal'
   */
  readonly type?: DialogType
  readonly title?: string
  readonly isDismissible?: boolean
  readonly onOpenChange?: (isOpen: boolean) => void
  readonly isKeyboardDismissDisabled?: boolean
}
