/** @file A tray for displaying notifications. */
import InboxIcon from '#/assets/inbox.svg'
import { Button, Popover, Text } from '#/components/AriaComponents'
import { Result } from '#/components/Result'
import { DialogTrigger, GridList } from '#/components/aria'
import { NotificationItem } from '#/layouts/NotificationTray/components/NotificationItem'
import { useComputedNotifications } from '#/layouts/NotificationTray/computedNotificationHooks'
import type { NotificationInfo } from '#/layouts/NotificationTray/types'
import { useText } from '#/providers/TextProvider'

const DIALOG_OFFSET = 16
const DIALOG_CROSS_OFFSET = 16

/** A button to show a list of notifications. */
export function NotificationTray() {
  const computedNotifications = useComputedNotifications()

  return (
    <DialogTrigger>
      <Button variant="icon" icon={InboxIcon} />
      <NotificationTrayDialog computedNotifications={computedNotifications} />
    </DialogTrigger>
  )
}

/** Props for a {@link NotificationTrayDialog}. */
interface NotificationTrayDialogProps {
  readonly computedNotifications: readonly NotificationInfo[]
}

/** Dialog to display notifications for a {@link NotificationTray}. */
function NotificationTrayDialog(props: NotificationTrayDialogProps) {
  const { computedNotifications } = props
  const { getText } = useText()

  return (
    <Popover placement="bottom right" offset={DIALOG_OFFSET} crossOffset={DIALOG_CROSS_OFFSET}>
      <div className="flex max-h-[90vh] flex-col overflow-y-auto">
        <Text.Heading level={3} variant="subtitle">
          {getText('notifications')}
        </Text.Heading>
        <NotificationTrayDialogInner computedNotifications={computedNotifications} />
      </div>
    </Popover>
  )
}

/** Dialog to display notifications for a {@link NotificationTray}. */
function NotificationTrayDialogInner(props: NotificationTrayDialogProps) {
  const { computedNotifications } = props
  const { getText } = useText()

  return (
    <GridList
      selectionMode="none"
      items={computedNotifications}
      renderEmptyState={() => (
        <Result centered className="min-h-10" title={getText('youAreAllCaughtUp')} />
      )}
    >
      {(info) => <NotificationItem {...info} />}
    </GridList>
  )
}
