/** @file Button bar for managing organization members. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import HorizontalMenuBar from '#/components/styled/HorizontalMenuBar'

import InviteUsersModal from '#/modals/InviteUsersModal'

// =============================
// === MembersSettingsTabBar ===
// =============================

/** Button bar for managing organization members. */
export default function MembersSettingsTabBar() {
  const { getText } = textProvider.useText()

  return (
    <HorizontalMenuBar>
      <ariaComponents.DialogTrigger>
        <ariaComponents.Button variant="cancel" rounding="full" size="small">
          {getText('inviteMembers')}
        </ariaComponents.Button>

        <InviteUsersModal relativeToTrigger />
      </ariaComponents.DialogTrigger>
    </HorizontalMenuBar>
  )
}
