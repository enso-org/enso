/** @file Modal for accepting or rejecting an invite to an organization. */
import { Outlet, useNavigate } from 'react-router'

import { LOGIN_PATH, SUPPORT_EMAIL, SUPPORT_EMAIL_URL } from '#/appUtils'
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
  const navigate = useNavigate()
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
    return <Outlet context={session} />
  } else {
    switch (user.newOrganizationInvite) {
      case 'pending': {
        return (
          <>
            <Outlet context={session} />
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
                    <Text>{getText('organizationInviteOpenMessage')}</Text>
                  </div>
                  <ButtonGroup className="w-min">
                    <Button
                      variant="tertiary"
                      onPress={async () => {
                        await acceptInvitation([])
                        navigate(LOGIN_PATH)
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
          </>
        )
      }
      case 'error': {
        return (
          <>
            <Outlet context={session} />
            <Dialog
              title={getText('organizationInviteTitle')}
              // For now, allow dismissing the modal as the user account is still usable.
              hideCloseButton
              modalProps={{ defaultOpen: true }}
            >
              <div className="text-center">
                <Text>{getText('organizationInviteMessage')}</Text>
                <Text className="text-sm font-bold">{user.newOrganizationName}</Text>
                <Text className="text-danger">
                  {getText('organizationInviteErrorMessage')}{' '}
                  {
                    <Button variant="link" href={SUPPORT_EMAIL_URL}>
                      {SUPPORT_EMAIL}
                    </Button>
                  }
                </Text>
              </div>
            </Dialog>
          </>
        )
      }
    }
  }
}
