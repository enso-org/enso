/** @file A toolbar containing chat and the user menu. */
import ChatIcon from '#/assets/chat.svg'
import DefaultUserIcon from '#/assets/default_user.svg'

import * as appUtils from '#/appUtils'

import * as billing from '#/hooks/billing'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import UserMenu from '#/layouts/UserMenu'

import * as ariaComponents from '#/components/AriaComponents'
import * as paywall from '#/components/Paywall'
import Button from '#/components/styled/Button'
import FocusArea from '#/components/styled/FocusArea'

import InviteUsersModal from '#/modals/InviteUsersModal'
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
  /** When `true`, the element occupies space in the layout but is not visible.
   * Defaults to `false`. */
  readonly invisible?: boolean
  readonly setIsHelpChatOpen: (isHelpChatOpen: boolean) => void
  readonly goToSettingsPage: () => void
  readonly onSignOut: () => void
  readonly onShareClick?: (() => void) | null | undefined
}

/** A toolbar containing chat and the user menu. */
export default function UserBar(props: UserBarProps) {
  const { invisible = false, setIsHelpChatOpen, onShareClick, goToSettingsPage, onSignOut } = props

  const { user } = authProvider.useFullUserSession()
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const { isFeatureUnderPaywall } = billing.usePaywall({ plan: user.plan })

  const shouldShowUpgradeButton =
    user.isOrganizationAdmin && user.plan !== Plan.enterprise && user.plan !== Plan.team
  const shouldShowPaywallButton = isFeatureUnderPaywall('inviteUser')
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
              <ariaComponents.Button
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
              <paywall.PaywallDialogButton feature="inviteUser" size="medium" variant="accent">
                {getText('invite')}
              </paywall.PaywallDialogButton>
            )}

            {shouldShowInviteButton && (
              <ariaComponents.DialogTrigger>
                <ariaComponents.Button size="medium" variant="accent">
                  {getText('invite')}
                </ariaComponents.Button>

                <InviteUsersModal />
              </ariaComponents.DialogTrigger>
            )}

            {shouldShowUpgradeButton && (
              <ariaComponents.Button variant="primary" size="medium" href={appUtils.SUBSCRIBE_PATH}>
                {getText('upgrade')}
              </ariaComponents.Button>
            )}

            {shouldShowShareButton && (
              <ariaComponents.Button
                size="medium"
                variant="accent"
                aria-label={getText('shareButtonAltText')}
                onPress={onShareClick}
              >
                {getText('share')}
              </ariaComponents.Button>
            )}
            <Button
              active
              mask={false}
              alt={getText('userMenuAltText')}
              image={user.profilePicture ?? DefaultUserIcon}
              buttonClassName="rounded-full after:rounded-full"
              className="h-row-h w-row-h rounded-full"
              onPress={() => {
                setModal(<UserMenu goToSettingsPage={goToSettingsPage} onSignOut={onSignOut} />)
              }}
            />
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
