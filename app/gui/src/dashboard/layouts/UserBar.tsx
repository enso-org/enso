/** @file A toolbar containing chat and the user menu. */
import { SUBSCRIBE_PATH } from '#/appUtils'
import ChatIcon from '#/assets/chat.svg'
import DefaultUserIcon from '#/assets/default_user.svg'
import Offline from '#/assets/offline_filled.svg'
import { Button, DialogTrigger, Text } from '#/components/AriaComponents'
import { PaywallDialogButton } from '#/components/Paywall'
import FocusArea from '#/components/styled/FocusArea'
import { usePaywall } from '#/hooks/billing'
import UserMenu from '#/layouts/UserMenu'
import InviteUsersModal from '#/modals/InviteUsersModal'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useText } from '#/providers/TextProvider'
import { Plan } from '#/services/Backend'
import { AnimatePresence, motion } from 'framer-motion'
import SvgMask from '../components/SvgMask'
import { useOffline } from '../hooks/offlineHooks'

/** Whether the chat button should be visible. Temporarily disabled. */
const SHOULD_SHOW_CHAT_BUTTON: boolean = false

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
  const { invisible = false, setIsHelpChatOpen, goToSettingsPage, onSignOut } = props

  const { user } = useFullUserSession()
  const { getText } = useText()
  const { isFeatureUnderPaywall } = usePaywall({ plan: user.plan })
  const { isOffline } = useOffline()

  const shouldShowUpgradeButton =
    user.isOrganizationAdmin && user.plan !== Plan.enterprise && user.plan !== Plan.team
  // eslint-disable-next-line no-restricted-syntax
  const shouldShowPaywallButton = (false as boolean) && isFeatureUnderPaywall('inviteUser')
  const shouldShowInviteButton =
    // eslint-disable-next-line no-restricted-syntax
    (false as boolean) && !shouldShowPaywallButton

  return (
    <FocusArea active={!invisible} direction="horizontal">
      {(innerProps) => (
        <div className="bg-primary/10 pt-0.5">
          <div
            className="flex h-[46px] shrink-0 cursor-default items-center gap-user-bar pl-icons-x pr-3"
            {...innerProps}
          >
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
              <Button variant="primary" size="medium" href={SUBSCRIBE_PATH}>
                {getText('upgrade')}
              </Button>
            )}

            <DialogTrigger>
              <Button
                size="custom"
                variant="icon"
                isActive
                icon={
                  <img src={user.profilePicture ?? DefaultUserIcon} className="aspect-square" />
                }
                aria-label={getText('userMenuLabel')}
                className="overflow-clip rounded-full opacity-100"
                contentClassName="size-8"
              />
              <UserMenu goToSettingsPage={goToSettingsPage} onSignOut={onSignOut} />
            </DialogTrigger>

            {/* Required for shortcuts to work. */}
            <div className="hidden">
              <UserMenu hidden goToSettingsPage={goToSettingsPage} onSignOut={onSignOut} />
            </div>
          </div>
        </div>
      )}
    </FocusArea>
  )
}
