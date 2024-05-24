/** @file Button bar for managing organization members. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import HorizontalMenuBar from '#/components/styled/HorizontalMenuBar'

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
  const { getText } = textProvider.useText()

  return (
    <HorizontalMenuBar>
      <ariaComponents.DialogTrigger>
        <ariaComponents.Button variant="cancel" rounded="full" size="small">
          {getText('inviteMembers')}
        </ariaComponents.Button>

        <InviteUsersModal />
      </ariaComponents.DialogTrigger>
    </HorizontalMenuBar>
  )
}
