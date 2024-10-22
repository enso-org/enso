/** @file Success screen for the "invite users" modal. */
import * as React from 'react'

import * as reactRouterDom from 'react-router-dom'

import ArrowRightIcon from '#/assets/arrow_right.svg'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import * as result from '#/components/Result'

// =================
// === Constants ===
// =================

/**
 * The number of emails to display in the success message.
 * If the number of emails is greater than this number, the message will be
 * x emails have been invited instead of listing all the emails.
 */
const MAX_EMAILS_DISPLAYED = 4

// ==========================
// === InviteUsersSuccess ===
// ==========================

/** Props for the {@link InviteUsersSuccess} component. */
export interface InviteUsersSuccessProps {
  readonly onClose?: () => void
  readonly emails: string[]
  readonly invitationLink: string
}

/** Success screen for the invite users modal. */
export function InviteUsersSuccess(props: InviteUsersSuccessProps) {
  const { onClose, emails, invitationLink } = props
  const { getText, locale } = textProvider.useText()
  const membersSearchParams = [
    ['cloud-ide_page', '"settings"'],
    ['cloud-ide_SettingsTab', '"members"'],
  ] as const

  const [searchParams, setSearchParams] = reactRouterDom.useSearchParams()

  const emailListFormatter = React.useMemo(
    () => new Intl.ListFormat(locale, { type: 'conjunction', style: 'long' }),
    [locale],
  )

  const isUserOnMembersPage =
    searchParams.has(membersSearchParams[0][0], membersSearchParams[0][1]) &&
    searchParams.has(membersSearchParams[1][0], membersSearchParams[1][1])

  return (
    <result.Result
      status="success"
      subtitle={getText('inviteUserLinkCopyDescription')}
      title={
        emails.length > MAX_EMAILS_DISPLAYED ?
          getText('inviteManyUsersSuccess', emails.length)
        : getText('inviteSuccess', emailListFormatter.format(emails))
      }
    >
      <ariaComponents.CopyBlock
        copyText={invitationLink}
        className="mb-6 mt-1"
        title={getText('copyInviteLink')}
      />

      {onClose && (
        <ariaComponents.ButtonGroup gap="medium" align={isUserOnMembersPage ? 'center' : 'end'}>
          {!isUserOnMembersPage && (
            <ariaComponents.Button
              variant="outline"
              icon={ArrowRightIcon}
              size="medium"
              iconPosition="end"
              onPressStart={onClose}
              onPress={() => {
                onClose()
                membersSearchParams.forEach(([key, value]) => {
                  searchParams.set(key, value)
                })
                setSearchParams(searchParams)
              }}
            >
              {getText('goToMembersPage')}
            </ariaComponents.Button>
          )}

          <ariaComponents.Button variant="primary" size="medium" onPress={onClose}>
            {getText('closeModalShortcut')}
          </ariaComponents.Button>
        </ariaComponents.ButtonGroup>
      )}
    </result.Result>
  )
}
