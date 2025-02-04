/** @file An item in the notification tray. */
import { Button, Text } from '#/components/AriaComponents'
import { GridListItem, ProgressBar } from '#/components/aria'
import type { NotificationInfo } from '#/layouts/NotificationTray/types'

/** An item in the notification tray. */
export function NotificationItem(props: NotificationInfo) {
  const { message, icon, progress, color, timestamp } = props

  return (
    <GridListItem>
      <div className="flex flex-col p-2">
        <div className="flex min-h-12 items-center gap-2 text-primary">
          <Button isDisabled isActive variant="icon" color={color} icon={icon} />
          <Text>{message}</Text>
          {timestamp != null && <Text color="disabled">{message}</Text>}
        </div>
        {progress != null && (
          <ProgressBar value={progress} maxValue={1}>
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
