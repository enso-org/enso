/** @file Modal for accepting or rejecting an invite to an organization. */
import { Alert } from '#/components/Alert'
import { Button } from '#/components/Button'
import { Dialog } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { Text } from '#/components/Text'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { SUPPORT_EMAIL, SUPPORT_EMAIL_URL } from '$/appUtils'
import { useBackends, useFullUserSession, useText } from '$/providers/react'
import { useMutation } from '@tanstack/react-query'
import * as React from 'react'
import * as z from 'zod'

/** Modal for accepting the terms of service. */
export function InvitedToOrganizationModal({ children }: React.PropsWithChildren) {
  const { getText } = useText()
  const { remoteBackend: backend } = useBackends()
  const { user } = useFullUserSession()
  const shouldDisplay = user.newOrganizationName != null && user.newOrganizationInvite != null

  const acceptInvitation = useMutation(
    backendMutationOptions(backend, 'acceptInvitation'),
  ).mutateAsync
  const declineInvitation = useMutation(
    backendMutationOptions(backend, 'declineInvitation'),
  ).mutateAsync

  if (!shouldDisplay) {
    return <>{children}</>
  } else {
    switch (user.newOrganizationInvite) {
      case 'pending': {
        return (
          <>
            {children}
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
                  <Button.Group className="w-min self-end">
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
                  </Button.Group>
                </Form>
              )}
            </Dialog>
          </>
        )
      }
      case 'error': {
        return (
          <>
            {children}
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
