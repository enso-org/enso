/** @file A toolbar containing chat and the user menu. */
import { SUBSCRIBE_PATH } from '#/appUtils'
import ChatIcon from '#/assets/chat.svg'
import DefaultUserIcon from '#/assets/default_user.svg'
import ArrowDownIcon from '#/assets/expand_arrow_down.svg'
import Offline from '#/assets/offline_filled.svg'
import { Button, DialogTrigger, Menu, Popover, Text } from '#/components/AriaComponents'
import { PaywallDialogButton } from '#/components/Paywall'
import SvgMask from '#/components/SvgMask'
import TOPBAR_LINKS from '#/configurations/topbarLinks.json' with { type: 'json' }
import { usePaywall } from '#/hooks/billing'
import { useOffline } from '#/hooks/offlineHooks'
import UserMenu from '#/layouts/UserMenu'
import InviteUsersModal from '#/modals/InviteUsersModal'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useText } from '#/providers/TextProvider'
import { Plan } from '#/services/Backend'
import { isAbsoluteUrl } from '#/utilities/url'
import type { TextId } from 'enso-common/src/text'
import { AnimatePresence, motion } from 'framer-motion'
import { z } from 'zod'

/** Whether the chat button should be visible. Temporarily disabled. */
const SHOULD_SHOW_CHAT_BUTTON: boolean = false

export const TOPBAR_LINKS_SCHEMA = z.object({
  items: z.array(
    z
      .object({
        name: z.custom<TextId>(),
        url: z.string().url(),
        menu: z.array(
          z.object({
            name: z.custom<TextId>().and(z.string()),
            url: z.string().url(),
          }),
        ),
      })
      .or(
        z.object({
          name: z.custom<TextId>(),
          menu: z.array(
            z.object({
              name: z.custom<TextId>().and(z.string()),
              url: z.string().url(),
            }),
          ),
        }),
      )
      .or(
        z.object({
          name: z.custom<TextId>().and(z.string()),
          url: z.string().url(),
        }),
      ),
  ),
})

/** Props for a {@link UserBar}. */
export interface UserBarProps {
  /**
   * When `true`, the element occupies space in the layout but is not visible.
   * Defaults to `false`.
   */
  readonly invisible?: boolean
  readonly setIsHelpChatOpen: (isHelpChatOpen: boolean) => void
  readonly goToSettingsPage: () => void
  readonly onSignOut: () => void
}

/** A toolbar containing chat and the user menu. */
export default function UserBar(props: UserBarProps) {
  const { setIsHelpChatOpen, goToSettingsPage, onSignOut } = props

  const { user } = useFullUserSession()
  const { getText } = useText()
  const { isFeatureUnderPaywall } = usePaywall({ plan: user.plan })
  const { isOffline } = useOffline()

  const shouldShowUpgradeButton =
    user.isOrganizationAdmin && user.plan !== Plan.enterprise && user.plan !== Plan.team

  const upgradeButtonVariant = user.plan === Plan.free ? 'primary' : 'outline'
  // eslint-disable-next-line no-restricted-syntax
  const shouldShowPaywallButton = (false as boolean) && isFeatureUnderPaywall('inviteUser')
  const shouldShowInviteButton =
    // eslint-disable-next-line no-restricted-syntax
    (false as boolean) && !shouldShowPaywallButton

  const topbarLinks = TOPBAR_LINKS_SCHEMA.parse(TOPBAR_LINKS)

  return (
    <div className="bg-primary/10 pt-0.5">
      <div className="flex h-full shrink-0 cursor-default items-center gap-user-bar pl-icons-x pr-2">
        <AnimatePresence initial={false}>
          {isOffline && (
            <motion.div
              // eslint-disable-next-line @typescript-eslint/no-magic-numbers
              initial={{ opacity: 0, x: 12 }}
              animate={{ opacity: 1, x: 0 }}
              // eslint-disable-next-line @typescript-eslint/no-magic-numbers
              exit={{ opacity: 0, x: 12 }}
              className="mr-2 flex items-center gap-2"
            >
              <SvgMask src={Offline} className="aspect-square w-4 flex-none" />
              <Text tooltip={getText('offlineToastMessage')} tooltipDisplay="always">
                {getText('youAreOffline')}
              </Text>
            </motion.div>
          )}
        </AnimatePresence>

        <UserBarHelpSection items={topbarLinks.items} />

        {SHOULD_SHOW_CHAT_BUTTON && (
          <Button
            variant="icon"
            size="custom"
            className="mr-1"
            icon={ChatIcon}
            aria-label={getText('openHelpChat')}
            onPress={() => {
              setIsHelpChatOpen(true)
            }}
          />
        )}

        {shouldShowPaywallButton && (
          <PaywallDialogButton feature="inviteUser" size="medium" variant="accent">
            {getText('invite')}
          </PaywallDialogButton>
        )}

        {shouldShowInviteButton && (
          <DialogTrigger>
            <Button size="medium" variant="accent">
              {getText('invite')}
            </Button>

            <InviteUsersModal />
          </DialogTrigger>
        )}

        {shouldShowUpgradeButton && (
          <Button variant={upgradeButtonVariant} size="medium" href={SUBSCRIBE_PATH}>
            {getText('upgrade')}
          </Button>
        )}

        <Popover.Trigger>
          <Button
            size="custom"
            variant="icon"
            isActive
            icon={<img src={user.profilePicture ?? DefaultUserIcon} className="aspect-square" />}
            aria-label={getText('userMenuLabel')}
            className="overflow-clip rounded-full opacity-100"
            contentClassName="size-8"
          />

          <UserMenu goToSettingsPage={goToSettingsPage} onSignOut={onSignOut} />
        </Popover.Trigger>

        {/* Required for shortcuts to work. */}
        <div className="hidden">
          <UserMenu hidden goToSettingsPage={goToSettingsPage} onSignOut={onSignOut} />
        </div>
      </div>
    </div>
  )
}

/**
 * Props for a {@link UserBarHelpSection}.
 */
export interface UserBarHelpSectionProps {
  readonly items: z.infer<typeof TOPBAR_LINKS_SCHEMA>['items']
}

/**
 * A section containing help buttons.
 */
export function UserBarHelpSection(props: UserBarHelpSectionProps) {
  const { items } = props
  const { getText } = useText()

  const getSafetyProps = (url: string) =>
    isAbsoluteUrl(url) ? { rel: 'opener', target: '_blank' } : {}

  return (
    <Button.Group gap="small" buttonVariants={{ variant: 'icon' }}>
      {items.map((item) => {
        if ('url' in item) {
          if ('menu' in item) {
            return (
              <Button.GroupJoin buttonVariants={{ variant: 'icon' }}>
                <Button href={item.url} {...getSafetyProps(item.url)}>
                  {getText(item.name)}
                </Button>

                <Menu.Trigger>
                  <Button icon={ArrowDownIcon} aria-label={getText('more')} />

                  <Menu>
                    {item.menu.map((menuItem) => (
                      <Menu.Item href={menuItem.url} {...getSafetyProps(menuItem.url)}>
                        {getText(menuItem.name)}
                      </Menu.Item>
                    ))}
                  </Menu>
                </Menu.Trigger>
              </Button.GroupJoin>
            )
          }
        } else {
          return (
            <Menu.Trigger>
              <Button icon={ArrowDownIcon}>{getText(item.name)}</Button>

              <Menu>
                {item.menu.map((menuItem) => (
                  <Menu.Item href={menuItem.url} {...getSafetyProps(menuItem.url)}>
                    {getText(menuItem.name)}
                  </Menu.Item>
                ))}
              </Menu>
            </Menu.Trigger>
          )
        }

        return (
          <Button href={item.url} {...getSafetyProps(item.url)}>
            {getText(item.name)}
          </Button>
        )
      })}
    </Button.Group>
  )
}
