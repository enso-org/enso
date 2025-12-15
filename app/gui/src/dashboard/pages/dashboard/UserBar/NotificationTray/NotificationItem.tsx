/** @file An item in the notification tray. */
import { CloseButton } from '#/components/Button'
import { Icon } from '#/components/Icon'
import { ProgressBar } from '#/components/ProgressBar'
import { Text } from '#/components/Text'
import { tv } from '#/utilities/tailwindVariants'
import { useText } from '$/providers/react'
import type { NotificationInfo } from './types'

const NOTIFICATION_ITEM_STYLES = tv({
  base: 'flex flex-col px-2',
  slots: {
    content: 'flex min-h-8 items-center gap-2 text-primary',
    contentPadding: 'grow',
    progressBarContainer: 'h-2 rounded-full bg-primary/10',
    progressBar: 'h-full rounded-full bg-accent transition-width duration-1000',
  },
})

/** Props for a {@link NotificationItem}. */
export interface NotificationItemProps extends NotificationInfo {
  readonly remove?: (() => Promise<void> | void) | undefined
}

/** An item in the notification tray. */
export function NotificationItem(props: NotificationItemProps) {
  const { message, icon, progress, color, timestamp, remove } = props
  const { locale, getText } = useText()
  const dateTime = timestamp != null ? new Date(timestamp) : undefined

  const styles = NOTIFICATION_ITEM_STYLES()

  return (
    <div className={styles.base()}>
      <div className={styles.content()}>
        <Icon color={color} icon={icon} />
        <Text>{message}</Text>
        <div className={styles.contentPadding()} />
        {dateTime != null && (
          <Text color="disabled">
            {dateTime.toLocaleString(locale, {
              ...(dateTime.toDateString() === new Date().toDateString() ?
                {}
              : { dateStyle: 'short' }),
              timeStyle: 'short',
            })}
          </Text>
        )}
        <CloseButton className={remove ? '' : 'invisible'} onPress={remove} />
      </div>
      {progress != null && (
        <ProgressBar progress={progress} aria-label={getText('notificationProgressLabel')} />
      )}
    </div>
  )
}
