/** @file Modal for accepting or rejecting an invite to an organization. */
import { Outlet } from 'react-router'

import * as z from 'zod'

import { SUPPORT_EMAIL, SUPPORT_EMAIL_URL } from '#/appUtils'
import { Alert, Button, ButtonGroup, Dialog, Form, Text } from '#/components/AriaComponents'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useAuth, useFullUserSession } from '#/providers/AuthProvider'
import { useRemoteBackend } from '#/providers/BackendProvider'
import { useText } from '#/providers/TextProvider'
import { useMutation } from '@tanstack/react-query'

// ==================================
// === InvitedToOrganizationModal ===
// ==================================

/** Modal for accepting the terms of service. */
export function InvitedToOrganizationModal() {
  const { getText } = useText()
  const { session } = useAuth()
  const backend = useRemoteBackend()
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
                <Form
                  schema={z.object({})}
                  className="flex flex-col gap-4"
                  onSubmit={async () => {
                    await acceptInvitation([])
                    close()
                  }}
                >
                  <div>
                    <Text disableLineHeightCompensation>{getText('organizationInvitePrefix')}</Text>
                    <Text disableLineHeightCompensation className="font-bold">
                      {user.newOrganizationName}
                    </Text>
                    <Text disableLineHeightCompensation>{getText('organizationInviteSuffix')}</Text>
                  </div>
                  <ButtonGroup className="w-min self-end">
                    <Button
                      variant="outline"
                      onPress={async () => {
                        await declineInvitation([user.email])
                        close()
                      }}
                    >
                      {getText('decline')}
                    </Button>
                    <Form.Submit variant="accent">{getText('accept')}</Form.Submit>
                  </ButtonGroup>
                </Form>
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
              <Text disableLineHeightCompensation>{getText('organizationInvitePrefix')}</Text>
              <Text disableLineHeightCompensation className="font-bold">
                {user.newOrganizationName}
              </Text>
              <Text disableLineHeightCompensation>{getText('organizationInviteErrorSuffix')}</Text>
              <Alert>
                {getText('organizationInviteErrorMessage')}{' '}
                <Button variant="link" href={SUPPORT_EMAIL_URL}>
                  {SUPPORT_EMAIL}
                </Button>
              </Alert>
            </Dialog>
          </>
        )
      }
    }
  }
}
