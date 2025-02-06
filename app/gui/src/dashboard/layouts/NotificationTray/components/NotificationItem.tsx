/** @file An item in the notification tray. */
import { Button, CloseButton, Text } from '#/components/AriaComponents'
import { GridListItem, ProgressBar } from '#/components/aria'
import type { NotificationInfo } from '#/layouts/NotificationTray/types'
import { useText } from '#/providers/TextProvider'

/** Props for a {@link NotificationItem}. */
export interface NotificationItemProps extends NotificationInfo {
  readonly remove: () => Promise<void> | void
}

/** An item in the notification tray. */
export function NotificationItem(props: NotificationItemProps) {
  const { message, icon, progress, color, timestamp, remove } = props
  const { locale } = useText()
  const dateTime = timestamp != null ? new Date(timestamp) : undefined

  return (
    <GridListItem>
      <div className="relative flex flex-col px-2">
        <CloseButton className="absolute right-0 top-0" onPress={remove} />
        <div className="flex min-h-8 items-center gap-2 text-primary">
          <Button isDisabled isActive variant="icon" color={color} icon={icon} />
          <Text>{message}</Text>
          <div className="grow" />
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
              <div className="h-2 rounded-full bg-primary/10">
                <div
                  className="h-full rounded-full bg-accent transition-width duration-1000"
                  style={{ width: percentage + '%' }}
                />
              </div>
            )}
          </ProgressBar>
        )}
      </div>
    </GridListItem>
  )
}
