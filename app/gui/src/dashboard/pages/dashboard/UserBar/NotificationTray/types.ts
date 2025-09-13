/** @file Types related to the `NotificationTray`. */
import type { IconProps } from '#/components/Icon'
import type { SvgUseIcon } from '#/components/types'

/** Information required to display a notification. */
export interface NotificationInfo {
  readonly id: string
  readonly message: string
  readonly icon: SvgUseIcon
  /** A number from 0 (not started) to 1 (finished). */
  readonly progress?: number | 'indeterminate' | undefined
  readonly color?: IconProps['color'] | undefined
  readonly timestamp?: number | undefined
  readonly showToast?: boolean | undefined
}
