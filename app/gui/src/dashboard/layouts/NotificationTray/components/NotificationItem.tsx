/** @file An item in the notification tray. */
import { Button, Text } from '#/components/AriaComponents'
import { GridListItem } from '#/components/aria'
import type { NotificationInfo } from '#/layouts/NotificationTray/types'

/** An item in the notification tray. */
export function NotificationItem(props: NotificationInfo) {
  const { message, icon, color } = props

  return (
    <GridListItem>
      <div className="flex min-h-12 items-center gap-2 p-2 text-primary">
        <Button isDisabled isActive variant="icon" color={color} icon={icon} />
        <Text>{message}</Text>
      </div>
    </GridListItem>
  )
}
