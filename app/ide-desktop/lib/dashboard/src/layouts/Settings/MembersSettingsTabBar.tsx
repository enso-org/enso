/** @file Button bar for managing organization members. */
import * as React from 'react'

import * as modalProvider from '#/providers/ModalProvider'

import * as aria from '#/components/aria'
import FocusArea from '#/components/styled/FocusArea'
import UnstyledButton from '#/components/styled/UnstyledButton'

import InviteUsersModal from '#/modals/InviteUsersModal'

// =============================
// === MembersSettingsTabBar ===
// =============================

/** Button bar for managing organization members. */
export default function MembersSettingsTabBar() {
  const { setModal } = modalProvider.useSetModal()

  return (
    <FocusArea direction="horizontal">
      {(ref, innerProps) => (
        <div ref={ref} className="flex gap-drive-bar" {...innerProps}>
          <UnstyledButton
            className="flex h-row items-center rounded-full bg-frame px-new-project-button-x"
            onPress={() => {
              setModal(<InviteUsersModal eventTarget={null} />)
            }}
          >
            <aria.Text className="text whitespace-nowrap font-semibold">Invite Members</aria.Text>
          </UnstyledButton>
        </div>
      )}
    </FocusArea>
  )
}
