/** @file Settings tab for viewing and editing organization members. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import MembersTable from '#/layouts/Settings/MembersTable'

import * as aria from '#/components/aria'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'
import HorizontalMenuBar from '#/components/styled/HorizontalMenuBar'
import UnstyledButton from '#/components/UnstyledButton'

import InviteUsersModal from '#/modals/InviteUsersModal'

// ==============================
// === MembersSettingsSection ===
// ==============================

/** Settings tab for viewing and editing organization members. */
export default function MembersSettingsSection() {
  const { backend } = backendProvider.useBackend()
  const { getText } = textProvider.useText()
  const { setModal } = modalProvider.useSetModal()
  const membersQuery = reactQuery.useQuery({
    queryKey: ['members'],
    queryFn: () => backend.listUsers(),
  })

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
