/** @file A tray for displaying notifications. */
import InboxIcon from '#/assets/inbox.svg'
import { Button } from '#/components/Button'
import { Popover } from '#/components/Dialog'
import { Result } from '#/components/Result'
import { StatusBadge } from '#/components/StatusBadge'
import SvgMask from '#/components/SvgMask'
import { Text } from '#/components/Text'
import { DialogTrigger, GridList, GridListItem } from '#/components/aria'
import { useText } from '$/providers/react'
import { useState } from 'react'
import { NotificationItem } from './NotificationItem'
import { useComputedNotifications, useNotificationState } from './computedNotificationHooks'
import type { NotificationInfo } from './types'

const DIALOG_OFFSET = 16
const DIALOG_CROSS_OFFSET = 16

/** A button to show a list of notifications. */
export function NotificationTray() {
  const { getText } = useText()
  const notificationState = useNotificationState()
  const { computedNotifications, removeComputedNotification } = notificationState
  useComputedNotifications(notificationState)

  const [lastOpenTimestamp, setLastOpenTimestamp] = useState(0)
  const hasUnreadNotifications = computedNotifications.some(
    (notification) => notification.timestamp != null && notification.timestamp > lastOpenTimestamp,
  )

  return (
    <DialogTrigger
      onOpenChange={(isOpen) => {
        if (isOpen) {
          setLastOpenTimestamp(Number(new Date()))
        }
      }}
    >
      <Button
        variant="icon"
        aria-label={getText('notifications')}
        icon={
          <StatusBadge color="danger" hidden={!hasUnreadNotifications}>
            <SvgMask className="size-4" src={InboxIcon} />
          </StatusBadge>
        }
      />
      <NotificationTrayDialog
        computedNotifications={computedNotifications}
        removeComputedNotification={removeComputedNotification}
      />
    </DialogTrigger>
  )
}

/** Props for a {@link NotificationTrayDialog}. */
interface NotificationTrayDialogProps {
  readonly computedNotifications: readonly NotificationInfo[]
  readonly removeComputedNotification: (id: string) => void
}

/** Dialog to display notifications for a {@link NotificationTray}. */
function NotificationTrayDialog(props: NotificationTrayDialogProps) {
  const { computedNotifications, removeComputedNotification } = props
  const { getText } = useText()

  return (
    <Popover placement="bottom right" offset={DIALOG_OFFSET} crossOffset={DIALOG_CROSS_OFFSET}>
      <div className="flex max-h-[90vh] flex-col overflow-y-auto">
        <Text.Heading level={3} variant="subtitle">
          {getText('notifications')}
        </Text.Heading>
        <NotificationTrayDialogInner
          computedNotifications={computedNotifications}
          removeComputedNotification={removeComputedNotification}
        />
      </div>
    </Popover>
  )
}

/** Dialog to display notifications for a {@link NotificationTray}. */
function NotificationTrayDialogInner(props: NotificationTrayDialogProps) {
  const { computedNotifications, removeComputedNotification } = props
  const { getText } = useText()

  return (
    <GridList
      aria-label={getText('notifications')}
      selectionMode="none"
      items={computedNotifications}
      renderEmptyState={() => (
        <Result centered className="min-h-10" title={getText('youAreAllCaughtUp')} />
      )}
    >
      {(info) => (
        <GridListItem>
          <NotificationItem
            {...info}
            remove={() => {
              removeComputedNotification(info.id)
            }}
          />
        </GridListItem>
      )}
    </GridList>
  )
}
