/** @file Button bar for managing organization members. */
import * as React from 'react'

import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import HorizontalMenuBar from '#/components/styled/HorizontalMenuBar'
import UnstyledButton from '#/components/UnstyledButton'

import InviteUsersModal from '#/modals/InviteUsersModal'

// =============================
// === MembersSettingsTabBar ===
// =============================

/** Button bar for managing organization members. */
export default function MembersSettingsTabBar() {
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const backend = backendProvider.useRemoteBackendStrict()

  return (
    <HorizontalMenuBar>
      <UnstyledButton
        className="flex h-row items-center rounded-full bg-frame px-new-project-button-x"
        onPress={() => {
          setModal(<InviteUsersModal backend={backend} eventTarget={null} />)
        }}
      >
        <aria.Text className="text whitespace-nowrap font-semibold">
          {getText('inviteMembers')}
        </aria.Text>
      </UnstyledButton>
    </HorizontalMenuBar>
  )
}
