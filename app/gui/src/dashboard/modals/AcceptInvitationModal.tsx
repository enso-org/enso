/** @file Modal for handling user organization invitation. */

import { Alert } from '#/components/Alert'
import { AlertDialog } from '#/components/AlertDialog'
import { Text } from '#/components/Text'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useBackends } from '$/providers/backends'
import { useText } from '$/providers/react'
import { useMutation } from '@tanstack/react-query'
import type * as backend from 'enso-common/src/services/Backend'
import { toast } from 'react-toastify'

/** Props for a {@link AcceptInvitationModal}. */
export interface AcceptInvitationModalProps {
  readonly invitation: backend.Invitation
}

/** Modal for handling subscription after the trial period ended. */
export function AcceptInvitationModal(props: AcceptInvitationModalProps) {
  const { invitation } = props
  const { organizationId, organizationName, userEmail } = invitation
  const { getText } = useText()
  const { remoteBackend } = useBackends()
  const onConfirm = useMutationCallback(
    backendMutationOptions(remoteBackend, 'updateUser', {
      onSuccess: () => {
        toast.success(getText('welcomeToTeam', organizationName))
      },
      onError: () => {
        toast.error(getText('invitationError'))
      },
    }),
  )
  const onCancel = useMutation(
    backendMutationOptions(remoteBackend, 'deleteInvitation', {
      meta: { invalidates: [['listInvitations']], awaitInvalidates: true },
    }),
  )

  return (
    <AlertDialog
      title={getText('pendingInvitationInfo')}
      modalProps={{ defaultOpen: true }}
      cancel={getText('decline')}
      confirm={getText('accept')}
      onConfirm={async () => {
        await onConfirm([{ organizationId }])
      }}
      onCancel={async () => {
        await onCancel.mutateAsync([userEmail])
      }}
    >
      <Text className="relative">{getText('invitationText', organizationName)}</Text>

      <Alert variant="outline" icon="warning">
        {getText('invitationAlert')}
      </Alert>
    </AlertDialog>
  )
}
