/** @file Types related to the `NotificationTray`. */
import type { SvgUseIcon } from '#/components/AriaComponents'
import type { IconProps } from '#/components/Icon'

/** Information required to display a notification. */
export interface NotificationInfo {
  readonly id: string
  readonly message: string
  readonly icon: SvgUseIcon
  /** A number from 0 (not started) to 1 (finished). */
  readonly progress?: number | 'indeterminate'
  readonly color?: IconProps['color']
  readonly timestamp?: number
  readonly showToast?: boolean
}
