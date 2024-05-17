/**
 * @file
 *
 * Success screen for the invite users modal.
 */
import * as React from 'react'

import * as reactRouter from 'react-router'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'

import * as appUtils from '#/appUtils'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import * as result from '#/components/Result'

/**
 * The number of emails to display in the success message.
 * If the number of emails is greater than this number, the message will be
 * x emails have been invited instead of listing all the emails.
 */
const MANY_EMAILS = 4

/**
 * Props for the InviteUsersSuccess component.
 */
export interface InviteUsersSuccessProps {
  readonly onClose?: () => void
  readonly emails: string[]
  readonly invitationLink: string
}

/**
 * Success screen for the invite users modal.
 */
export function InviteUsersSuccess(props: InviteUsersSuccessProps) {
  const { onClose, emails, invitationLink } = props
  const { getText, locale } = textProvider.useText()
  const membersSearchParams = 'cloud-ide_page="settings"&cloud-ide_SettingsTab="members"'
  const membersPageUrl = reactRouter.useHref({
    pathname: appUtils.DASHBOARD_PATH,
    search: membersSearchParams,
  })
  const { search } = reactRouter.useLocation()

  const emailListFormatter = React.useMemo(
    () => new Intl.ListFormat(locale, { type: 'conjunction', style: 'long' }),
    [locale]
  )

  const isUserOnMembersPage = decodeURIComponent(search) === `?${membersSearchParams}`

  return (
    <result.Result
      icon={false}
      title={
        emails.length > MANY_EMAILS
          ? getText('inviteManyUsersSuccess', emails.length)
          : getText('inviteSuccess', emailListFormatter.format(emails))
      }
      subtitle={'You can share the invite link with others to join the team.'}
    >
      <ariaComponents.CopyBlock
        copyText={invitationLink}
        className="mb-6"
        title={getText('copyInviteLink')}
      />

      {onClose && (
        <ariaComponents.ButtonGroup gap="medium" align={isUserOnMembersPage ? 'center' : 'end'}>
          {!isUserOnMembersPage && (
            <ariaComponents.Button
              variant="cancel"
              icon={ArrowRightIcon}
              size="medium"
              iconPosition="end"
              onPressStart={onClose}
              href={membersPageUrl}
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
