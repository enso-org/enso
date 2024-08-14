/** @file Modal for accepting or rejecting an invite to an organization. */
import * as router from 'react-router'

import { Button, ButtonGroup, Dialog, Text } from '#/components/AriaComponents'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useAuth, useFullUserSession } from '#/providers/AuthProvider'
import { useRemoteBackendStrict } from '#/providers/BackendProvider'
import { useText } from '#/providers/TextProvider'
import { useMutation } from '@tanstack/react-query'

// ==================================
// === InvitedToOrganizationModal ===
// ==================================

/** Modal for accepting the terms of service. */
export function InvitedToOrganizationModal() {
  const { getText } = useText()
  const { session } = useAuth()
  const backend = useRemoteBackendStrict()
  const { user } = useFullUserSession()
  const shouldDisplay = user.newOrganizationName != null && user.newOrganizationInvite != null

  const acceptInvitation = useMutation(
    backendMutationOptions(backend, 'acceptInvitation'),
  ).mutateAsync
  const declineInvitation = useMutation(
    backendMutationOptions(backend, 'declineInvitation'),
  ).mutateAsync

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
        {({ close }) => (
          <div className="flex flex-col items-center gap-4">
            <div className="text-center">
              <Text>{getText('organizationInviteMessage')}</Text>
              <Text className="text-sm font-bold">{user.newOrganizationName}</Text>
              <Text>{statusMessage}</Text>
            </div>
            <ButtonGroup className="w-min">
              <Button
                variant="tertiary"
                onPress={async () => {
                  await acceptInvitation([])
                  close()
                }}
              >
                {getText('accept')}
              </Button>
              <Button
                variant="outline"
                onPress={async () => {
                  await declineInvitation([user.email])
                  close()
                }}
              >
                {getText('decline')}
              </Button>
            </ButtonGroup>
          </div>
        )}
      </Dialog>
    )
  }
}
