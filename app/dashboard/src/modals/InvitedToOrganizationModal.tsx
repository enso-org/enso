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
  const shouldDisplay = user.newOrganizationName != null && user.newOrganizationInvite != null

  if (!shouldDisplay) {
    return <router.Outlet context={session} />
  } else {
    const status = user.newOrganizationInvite
    const statusMessage = (() => {
      switch (status) {
        case 'pending': {
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
  }
}
