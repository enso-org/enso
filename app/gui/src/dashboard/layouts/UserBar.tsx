/** @file A toolbar containing chat and the user menu. */
import { SUBSCRIBE_PATH } from '#/appUtils'
import ChatIcon from '#/assets/chat.svg'
import DefaultUserIcon from '#/assets/default_user.svg'
import { Button, DialogTrigger } from '#/components/AriaComponents'
import { PaywallDialogButton } from '#/components/Paywall'
import FocusArea from '#/components/styled/FocusArea'
import { usePaywall } from '#/hooks/billing'
import UserMenu from '#/layouts/UserMenu'
import InviteUsersModal from '#/modals/InviteUsersModal'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useText } from '#/providers/TextProvider'
import { Plan } from '#/services/Backend'

// =================
// === Constants ===
// =================

/** Whether the chat button should be visible. Temporarily disabled. */
const SHOULD_SHOW_CHAT_BUTTON: boolean = false

// ===============
// === UserBar ===
// ===============

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
  readonly onShareClick?: (() => void) | null | undefined
}

/** A toolbar containing chat and the user menu. */
export default function UserBar(props: UserBarProps) {
  const { invisible = false, setIsHelpChatOpen, onShareClick, goToSettingsPage, onSignOut } = props

  const { user } = useFullUserSession()
  const { getText } = useText()
  const { isFeatureUnderPaywall } = usePaywall({ plan: user.plan })

  const shouldShowUpgradeButton =
    user.isOrganizationAdmin && user.plan !== Plan.enterprise && user.plan !== Plan.team
  // eslint-disable-next-line no-restricted-syntax
  const shouldShowPaywallButton = (false as boolean) && isFeatureUnderPaywall('inviteUser')
  // FIXME[sb]: Re-enable when they are wanted again.
  // eslint-disable-next-line no-restricted-syntax
  const shouldShowShareButton = (false as boolean) && onShareClick != null
  const shouldShowInviteButton =
    // eslint-disable-next-line no-restricted-syntax
    (false as boolean) && !shouldShowShareButton && !shouldShowPaywallButton

  return (
    <FocusArea active={!invisible} direction="horizontal">
      {(innerProps) => (
        <div className="bg-primary/5 pt-0.5">
          <div
            className="flex h-[46px] shrink-0 cursor-default items-center gap-user-bar pl-icons-x pr-3"
            {...innerProps}
          >
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

            {shouldShowShareButton && (
              <Button
                size="medium"
                variant="accent"
                aria-label={getText('shareButtonAltText')}
                onPress={onShareClick}
              >
                {getText('share')}
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
