/** @file A user display with a popover for more information. */
import { Button, CopyButton } from '#/components/Button'
import { Popover } from '#/components/Dialog'
import { TEXT_WITH_ICON } from '#/components/patterns'
import { ProfilePicture } from '#/components/ProfilePicture'
import { Text } from '#/components/Text'
import { useText } from '$/providers/react'
import type { OtherUser } from 'enso-common/src/services/Backend'
import { twMerge } from 'tailwind-merge'

/** Props for a {@link UserWithPopover}. */
export interface UserWithPopoverProps {
  readonly user: OtherUser
  readonly className?: string
}

/** A user display with a popover for more information. */
export function UserWithPopover(props: UserWithPopoverProps) {
  const { user, className } = props

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
        className={twMerge('min-w-0', className)}
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
