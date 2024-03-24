/**
 * @file
 * Contains the types for the Dialog component.
 */
import type * as reactAriaComponents from 'react-aria-components'

/**
 *
 */
export type DialogType = 'fullscreen' | 'modal' | 'popover'

/**
 * The props for the Dialog component.
 */
export interface DialogProps extends reactAriaComponents.DialogProps {
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

/**
 * The props for the DialogTrigger component.
 */
export interface DialogTriggerProps extends reactAriaComponents.DialogTriggerProps {}
