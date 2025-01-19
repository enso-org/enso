/** @file Types related to the `NotificationTray`. */
import type { ButtonProps } from '#/components/AriaComponents'

/** Information required to display a notification. */
export interface NotificationInfo {
  readonly id: string
  readonly message: string
  readonly icon: string
  /** A number from 0 (not started) to 1 (finished). */
  readonly progress?: number
  readonly color?: ButtonProps['color']
}
