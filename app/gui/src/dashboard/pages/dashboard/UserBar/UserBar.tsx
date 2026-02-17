/** @file A toolbar containing chat and the user menu. */
import ArrowDownIcon from '#/assets/expand_arrow_down.svg'
import Offline from '#/assets/offline_filled.svg'
import { Button } from '#/components/Button'
import { Dialog, Popover } from '#/components/Dialog'
import { Menu } from '#/components/Menu'
import { ProfilePicture } from '#/components/ProfilePicture'
import { ProgressBar } from '#/components/ProgressBar'
import SvgMask from '#/components/SvgMask'
import { Text } from '#/components/Text'
import { VisualTooltip } from '#/components/VisualTooltip'
import TOPBAR_LINKS from '#/configurations/topbarLinks.json' with { type: 'json' }
import { backendQueryOptions } from '#/hooks/backendHooks'
import { usePaywall } from '#/hooks/billing'
import { useOffline } from '#/hooks/offlineHooks'
import InviteUsersModal from '#/modals/InviteUsersModal'
import { rfc3339DurationProgress } from '#/utilities/time'
import { isAbsoluteUrl } from '#/utilities/url'
import { SUBSCRIBE_PATH } from '$/appUtils'
import { useBackends, useFullUserSession, useText } from '$/providers/react'
import { useQuery } from '@tanstack/react-query'
import { Plan } from 'enso-common/src/services/Backend'
import type { TextId } from 'enso-common/src/text'
import { toReadableIsoString } from 'enso-common/src/utilities/data/dateTime'
import { twJoin } from 'tailwind-merge'
import { z } from 'zod'
import { NotificationTray } from './NotificationTray'
import { UserMenu } from './UserMenu'

const TEXT_ID_SCHEMA = z.custom<TextId>((s) => typeof s === 'string')

// eslint-disable-next-line react-refresh/only-export-components
export const TOPBAR_LINKS_SCHEMA = z.object({
  items: z.array(
    z
      .object({
        name: TEXT_ID_SCHEMA,
        url: z.string().url().optional(),
        menu: z
          .array(
            z.object({
              name: TEXT_ID_SCHEMA,
              url: z.string().url(),
            }),
          )
          .optional(),
      })
      .refine((obj) => 'url' in obj || 'menu' in obj),
  ),
})

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
  const { isFeatureUnderPaywall } = usePaywall({ plan: user.plan })
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
  const topbarLinks = TOPBAR_LINKS_SCHEMA.parse(TOPBAR_LINKS)

  return (
    <div className="pt-0.5">
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
        <div className={twJoin('flex', isCurrentlyTrialing ? 'md:hidden' : 'sm:hidden')}>
          <Popover.Trigger>
            <Button variant="icon" icon="help" aria-label={getText('help')} />
            <Popover size="auto">
              <UserBarHelpSection items={topbarLinks.items} className="flex-col" />
            </Popover>
          </Popover.Trigger>
        </div>
        <UserBarHelpSection
          items={topbarLinks.items}
          className={twJoin('hidden', isCurrentlyTrialing ? 'md:flex' : 'sm:flex')}
        />
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

/** Props for a {@link UserBarHelpSection}. */
export interface UserBarHelpSectionProps {
  readonly items: z.infer<typeof TOPBAR_LINKS_SCHEMA>['items']
  readonly className?: string
}

/** A section containing help buttons. */
export function UserBarHelpSection(props: UserBarHelpSectionProps) {
  const { items, className } = props
  const { getText } = useText()

  const getSafetyProps = (url: string) =>
    isAbsoluteUrl(url) ? { rel: 'opener', target: '_blank' } : {}

  return (
    <Button.Group gap="small" buttonVariants={{ variant: 'icon' }} className={className}>
      {items.map((item) => {
        if (item.url != null) {
          const button = (
            <Button key={item.name} href={item.url} {...getSafetyProps(item.url)}>
              {getText(item.name)}
            </Button>
          )
          if (item.menu == null) {
            return button
          }
          return (
            <Button.GroupJoin key={item.name} buttonVariants={{ variant: 'icon' }}>
              {button}
              <Menu.Trigger>
                <Button icon={ArrowDownIcon} aria-label={getText('more')} />
                <Menu placement="bottom right">
                  {item.menu.map((menuItem) => (
                    <Menu.Item
                      key={menuItem.name}
                      href={menuItem.url}
                      {...getSafetyProps(menuItem.url)}
                    >
                      {getText(menuItem.name)}
                    </Menu.Item>
                  ))}
                </Menu>
              </Menu.Trigger>
            </Button.GroupJoin>
          )
        }
        return null
      })}
    </Button.Group>
  )
}
