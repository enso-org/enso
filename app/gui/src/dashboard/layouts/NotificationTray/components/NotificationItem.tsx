/** @file An item in the notification tray. */
import { Button, CloseButton, Text } from '#/components/AriaComponents'
import { GridListItem, ProgressBar } from '#/components/aria'
import type { NotificationInfo } from '#/layouts/NotificationTray/types'
import { useText } from '#/providers/TextProvider'
import { tv } from '#/utilities/tailwindVariants'

const NOTIFICATION_ITEM_STYLES = tv({
  base: 'relative flex flex-col px-2',
  slots: {
    closeButton: 'absolute right-0 top-0',
    content: 'flex min-h-8 items-center gap-2 text-primary',
    contentPadding: 'grow',
    progressBarContainer: 'h-2 rounded-full bg-primary/10',
    progressBar: 'h-full rounded-full bg-accent transition-width duration-1000',
  },
})

/** Props for a {@link NotificationItem}. */
export interface NotificationItemProps extends NotificationInfo {
  readonly remove?: () => Promise<void> | void
}

/** An item in the notification tray. */
export function NotificationItem(props: NotificationItemProps) {
  const { message, icon, progress, color, timestamp, remove } = props
  const { locale } = useText()
  const dateTime = timestamp != null ? new Date(timestamp) : undefined

  const styles = NOTIFICATION_ITEM_STYLES()

  return (
    <GridListItem>
      <div className={styles.base()}>
        {remove && <CloseButton className={styles.closeButton()} onPress={remove} />}
        <div className={styles.content()}>
          <Button isDisabled isActive variant="icon" color={color} icon={icon} />
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
        </div>
        {progress != null && (
          <ProgressBar
            isIndeterminate={progress === 'indeterminate'}
            value={progress === 'indeterminate' ? 0 : progress}
            maxValue={1}
          >
            {({ percentage }) => (
              <div className={styles.progressBarContainer()}>
                <div className={styles.progressBar()} style={{ width: percentage + '%' }} />
              </div>
            )}
          </ProgressBar>
        )}
      </div>
    </GridListItem>
  )
}
