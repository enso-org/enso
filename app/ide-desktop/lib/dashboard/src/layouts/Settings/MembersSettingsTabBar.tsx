/** @file Button bar for managing organization members. */
import * as React from 'react'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import HorizontalMenuBar from '#/components/styled/HorizontalMenuBar'
import UnstyledButton from '#/components/UnstyledButton'

import InviteUsersModal from '#/modals/InviteUsersModal'

import type Backend from '#/services/Backend'

// =============================
// === MembersSettingsTabBar ===
// =============================

/** Props for a {@link MembersSettingsTabBar}. */
export interface MembersSettingsTabBarProps {
  readonly backend: Backend
}

/** Button bar for managing organization members. */
export default function MembersSettingsTabBar(props: MembersSettingsTabBarProps) {
  const { backend } = props
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()

  return (
    <HorizontalMenuBar>
      <UnstyledButton
        variant="bar"
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
