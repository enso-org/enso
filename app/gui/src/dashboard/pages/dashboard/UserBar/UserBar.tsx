/** @file A toolbar containing chat and the user menu. */
import Offline from '#/assets/offline_filled.svg'
import { Button } from '#/components/Button'
import { Dialog, Popover } from '#/components/Dialog'
import { ProfilePicture } from '#/components/ProfilePicture'
import { ProgressBar } from '#/components/ProgressBar'
import SvgMask from '#/components/SvgMask'
import { Text } from '#/components/Text'
import { VisualTooltip } from '#/components/VisualTooltip'
import { backendQueryOptions } from '#/hooks/backendHooks'
import { useOffline } from '#/hooks/offlineHooks'
import InviteUsersModal from '#/modals/InviteUsersModal'
import { rfc3339DurationProgress } from '#/utilities/time'
import { SUBSCRIBE_PATH } from '$/appUtils'
import {
  useBackends,
  useFullUserSession,
  useIsFeatureUnderPaywall,
  useText,
} from '$/providers/react'
import { useQuery } from '@tanstack/react-query'
import { Plan } from 'enso-common/src/services/Backend'
import { toReadableIsoString } from 'enso-common/src/utilities/data/dateTime'
import { NotificationTray } from './NotificationTray'
import { UserMenu } from './UserMenu'

/** Props for a {@link UserBar}. */
export interface UserBarProps {
  readonly goToSettingsPage: () => void
  readonly onSignOut: () => void
}

/** A toolbar containing chat and the user menu. */
export function UserBar(props: UserBarProps) {
  const { goToSettingsPage, onSignOut } = props

  const { user } = useFullUserSession()
  const { getText } = useText()
  const isFeatureUnderPaywall = useIsFeatureUnderPaywall()
  const { isOffline } = useOffline()
  const { remoteBackend } = useBackends()
  const { data: organization } = useQuery(
    backendQueryOptions(remoteBackend, 'getOrganization', [], {
      enabled: user.isOrganizationAdmin,
    }),
  )
  const subscription = user.isOrganizationAdmin ? organization?.subscription : null
  const trialProgress =
    (
      subscription?.trialEnd != null &&
      new Date(subscription.trialEnd) > new Date() &&
      subscription.trialStart != null
    ) ?
      rfc3339DurationProgress(subscription.trialStart, subscription.trialEnd)
    : null
  const trialText =
    trialProgress == null ? null
    : trialProgress.daysLeft > 0 ? getText('xDaysLeftInTrial', trialProgress.daysLeft)
    : trialProgress.hoursLeft > 0 ? getText('xHoursLeftInTrial', trialProgress.hoursLeft)
    : getText('lessThanOneHourLeftInTrial')
  const isCurrentlyTrialing = trialProgress != null && subscription?.trialEnd != null

  const shouldShowInviteButton = !isFeatureUnderPaywall('inviteUser')
  const shouldShowUpgradeButton = user.isOrganizationAdmin && user.plan === Plan.free
  const upgradeButtonVariant = user.plan === Plan.free ? 'primary' : 'outline'

  return (
    <div className="flex-shrink-0 pt-0.5">
      <div className="flex h-full shrink-0 cursor-default items-center gap-user-bar pl-icons-x">
        {isOffline && (
          <div className="mr-2 flex items-center gap-2">
            <SvgMask src={Offline} className="aspect-square w-4 flex-none" />
            <Text tooltip={getText('offlineToastMessage')} tooltipDisplay="always">
              {getText('youAreOffline')}
            </Text>
          </div>
        )}
        {isCurrentlyTrialing && (
          <VisualTooltip
            className="relative px-2"
            tooltip={getText(
              'yourSubscriptionExpiresAtX',
              toReadableIsoString(new Date(subscription.trialEnd)),
            )}
          >
            <Text className="opacity-0">{trialText}</Text>
            <ProgressBar
              progress={trialProgress.fraction}
              variant="clipped"
              className="absolute inset-0"
              progressBarClassName="bg-accent/50"
              aria-label={getText('trialProgressLabel')}
            />
            <Text className="absolute inset-0 mx-2 cursor-help text-center">{trialText}</Text>
          </VisualTooltip>
        )}
        {shouldShowInviteButton && (
          <Dialog.Trigger>
            <Button size="medium" variant="outline">
              {getText('invite')}
            </Button>

            <InviteUsersModal />
          </Dialog.Trigger>
        )}
        {shouldShowUpgradeButton && (
          <Button variant={upgradeButtonVariant} size="medium" href={SUBSCRIBE_PATH}>
            {getText('upgrade')}
          </Button>
        )}
        <NotificationTray />
        <Popover.Trigger>
          <Button
            size="custom"
            variant="icon"
            icon={<ProfilePicture picture={user.profilePicture} name={user.name} />}
            className="ml-2"
            aria-label={getText('userMenuLabel')}
          />
          <UserMenu goToSettingsPage={goToSettingsPage} onSignOut={onSignOut} />
        </Popover.Trigger>
      </div>
    </div>
  )
}
