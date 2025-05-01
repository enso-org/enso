/** @file A user display with a popover for more information. */
import { Button, CopyButton, Popover, Text } from '#/components/AriaComponents'
import { TEXT_WITH_ICON } from '#/components/patterns'
import { ProfilePicture } from '#/components/ProfilePicture'
import { useText } from '#/providers/TextProvider'
import type { OtherUser } from '#/services/Backend'

/** Props for a {@link UserWithPopover}. */
export interface UserWithPopoverProps {
  readonly user: OtherUser
}

/** A user display with a popover for more information. */
export function UserWithPopover(props: UserWithPopoverProps) {
  const { user } = props

  const { getText } = useText()

  return (
    <Popover.Trigger>
      <Button
        variant="ghost"
        size="xxsmall"
        icon={
          <ProfilePicture
            picture={user.profilePicture}
            name={user.name}
            size="xxsmall"
            className="-mt-0.5"
          />
        }
        className="min-w-0"
      >
        <Text variant="body-sm" truncate="1" nowrap>
          {user.name}
        </Text>
      </Button>

      <Popover>
        <div className={TEXT_WITH_ICON().base({ verticalAlign: 'top' })}>
          <ProfilePicture
            picture={user.profilePicture}
            name={user.name}
            className={TEXT_WITH_ICON().icon()}
          />

          <div
            className={TEXT_WITH_ICON().text({
              className: 'flex flex-col items-start',
            })}
          >
            <Text.Group>
              <Text variant="body" className="leading-[1.2]" truncate="3">
                {user.name}
              </Text>

              <Button.Group verticalAlign="center">
                <Button
                  variant="link"
                  size="small"
                  icon="email"
                  className="min-w-0"
                  tooltip={getText('sendEmail')}
                  href={`mailto:${user.email}`}
                >
                  {user.email}
                </Button>

                <CopyButton size="xsmall" className="min-w-0" copyText={user.email} />
              </Button.Group>
            </Text.Group>
          </div>
        </div>
      </Popover>
    </Popover.Trigger>
  )
}
