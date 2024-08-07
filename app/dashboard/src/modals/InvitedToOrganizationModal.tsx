/** @file Modal for accepting or rejecting an invite to an organization. */
import * as router from 'react-router'

import { Dialog } from '#/components/AriaComponents'
import { useAuth, useFullUserSession } from '#/providers/AuthProvider'
import { useText } from '#/providers/TextProvider'

// ==================================
// === InvitedToOrganizationModal ===
// ==================================

/** Modal for accepting the terms of service. */
export function InvitedToOrganizationModal() {
  const { getText } = useText()
  const { session } = useAuth()
  const { user } = useFullUserSession()
  const shouldDisplay = user.newOrganization != null

  if (shouldDisplay) {
    const status = user.newOrganization.status
    const statusMessage = (() => {
      switch (status) {
        case 'open': {
          return getText('organizationInviteOpenMessage')
        }
        case 'error': {
          return getText('organizationInviteErrorMessage')
        }
      }
    })()
    return (
      <Dialog
        title={getText('organizationInviteTitle')}
        isKeyboardDismissDisabled
        isDismissable={false}
        hideCloseButton
        modalProps={{ defaultOpen: true }}
      >
        <span>{getText('organizationInviteMessage')}</span>
        <span>{statusMessage}</span>
      </Dialog>
    )
  } else {
    return <router.Outlet context={session} />
  }
}
