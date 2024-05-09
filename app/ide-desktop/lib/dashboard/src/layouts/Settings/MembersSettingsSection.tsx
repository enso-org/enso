/** @file Settings tab for viewing and editing organization members. */
import * as React from 'react'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import MembersTable from '#/layouts/Settings/MembersTable'

import * as aria from '#/components/aria'
import HorizontalMenuBar from '#/components/styled/HorizontalMenuBar'
import UnstyledButton from '#/components/UnstyledButton'

import InviteUsersModal from '#/modals/InviteUsersModal'

// ==============================
// === MembersSettingsSection ===
// ==============================

/** Settings tab for viewing and editing organization members. */
export default function MembersSettingsSection() {
  const { getText } = textProvider.useText()
  const { setModal } = modalProvider.useSetModal()

  return (
    <>
      <HorizontalMenuBar>
        <UnstyledButton
          className="flex h-row items-center rounded-full bg-frame px-new-project-button-x"
          onPress={event => {
            const rect = event.target.getBoundingClientRect()
            const position = { pageX: rect.left, pageY: rect.top }
            setModal(<InviteUsersModal event={position} />)
          }}
        >
          <aria.Text className="text whitespace-nowrap font-semibold">
            {getText('inviteMembers')}
          </aria.Text>
        </UnstyledButton>
      </HorizontalMenuBar>
      <MembersTable allowDelete />
    </>
  )
}
