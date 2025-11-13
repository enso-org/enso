/** @file Settings tab for viewing and editing organization members. */
import { Button, CopyButton } from '#/components/Button'
import { Dialog } from '#/components/Dialog'
import * as paywall from '#/components/Paywall'
import { Scroller } from '#/components/Scroller'
import { Text } from '#/components/Text'
import { backendMutationOptions, backendQueryOptions } from '#/hooks/backendHooks'
import * as billingHooks from '#/hooks/billing'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import InviteUsersModal from '#/modals/InviteUsersModal'
import { setModal } from '#/providers/ModalProvider'
import * as authProvider from '$/providers/react'
import { useBackends, useText } from '$/providers/react'
import { useMutation, useSuspenseQueries } from '@tanstack/react-query'
import type * as backendModule from 'enso-common/src/services/Backend'
import type { RemoteBackend } from 'enso-common/src/services/RemoteBackend'

const LIST_USERS_STALE_TIME_MS = 60_000

/** Settings tab for viewing and editing organization members. */
export default function MembersSettingsSection() {
  const { getText } = useText()
  const { remoteBackend: backend } = useBackends()
  const { user } = authProvider.useFullUserSession()

  const { isFeatureUnderPaywall, getFeature } = billingHooks.usePaywall({ plan: user.plan })

  const [{ data: members }, { data: invitations }] = useSuspenseQueries({
    queries: [
      backendQueryOptions(backend, 'listUsers', [], { staleTime: LIST_USERS_STALE_TIME_MS }),
      backendQueryOptions(backend, 'listInvitations', [], { staleTime: LIST_USERS_STALE_TIME_MS }),
    ],
  })

  const isUnderPaywall = isFeatureUnderPaywall('inviteUserFull')
  const feature = getFeature('inviteUser')

  const seatsLeft = isUnderPaywall ? invitations.availableLicenses : null
  const seatsTotal = isUnderPaywall ? invitations.maxLicenses : feature.meta.maxSeats
  const isAdmin = user.isOrganizationAdmin

  return (
    <>
      {isAdmin && (
        <Button.Group className="flex-initial" verticalAlign="center">
          <Dialog.Trigger>
            <Button variant="outline" rounded="full" size="medium">
              {getText('inviteMembers')}
            </Button>

            <InviteUsersModal />
          </Dialog.Trigger>

          {seatsLeft != null && (
            <div className="flex items-center gap-1">
              <Text>
                {seatsLeft <= 0 ?
                  getText('noSeatsLeft')
                : getText('seatsLeft', seatsLeft, seatsTotal)}
              </Text>

              <paywall.PaywallDialogButton
                feature="inviteUserFull"
                variant="link"
                showIcon={false}
              />
            </div>
          )}
        </Button.Group>
      )}

      <Scroller
        scrollbar
        orientation="vertical"
        className="min-h-0 flex-1"
        shadowStartClassName="top-8"
      >
        <table className="table-fixed self-start rounded-rows">
          <thead className="sticky top-0 z-1 bg-dashboard">
            <tr className="h-row">
              <th className="min-w-48 max-w-80 border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0">
                {getText('name')}
              </th>
              <th className="w-48 border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0">
                {getText('status')}
              </th>
            </tr>
          </thead>
          <tbody className="select-text">
            {members.map((member) => (
              <tr key={member.userId} className="group h-row rounded-rows-child">
                <td className="min-w-48 max-w-80 border-x-2 border-transparent bg-clip-padding px-4 py-1 first:rounded-l-full last:rounded-r-full last:border-r-0">
                  <Text truncate="1" className="block">
                    {member.email}
                  </Text>
                  <Text truncate="1" className="block text-2xs text-primary/40">
                    {member.name}
                  </Text>
                </td>
                <td className="border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
                  <div className="flex flex-col">
                    {getText('active')}
                    {member.email !== user.email && isAdmin && (
                      <Button.Group gap="small" className="mt-0.5">
                        <RemoveMemberButton
                          backend={backend}
                          userId={member.userId}
                          userEmail={member.email}
                          userUsername={member.name}
                        />
                      </Button.Group>
                    )}
                  </div>
                </td>
              </tr>
            ))}
            {invitations.invitations.map((invitation) => (
              <tr key={invitation.userEmail} className="group h-row rounded-rows-child">
                <td className="border-x-2 border-transparent bg-clip-padding px-4 py-1 first:rounded-l-full last:rounded-r-full last:border-r-0">
                  <span className="block text-sm">{invitation.userEmail}</span>
                </td>
                <td className="border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
                  <div className="flex flex-col">
                    {getText('pendingInvitation')}
                    {isAdmin && (
                      <Button.Group gap="small" className="mt-0.5">
                        <CopyButton
                          size="custom"
                          // eslint-disable-next-line @typescript-eslint/naming-convention, camelcase
                          copyText={`enso://auth/registration?=${new URLSearchParams({ organization_id: invitation.organizationId }).toString()}`}
                          aria-label={getText('copyInviteLink')}
                          copyIcon={false}
                        >
                          {getText('copyInviteLink')}
                        </CopyButton>

                        <ResendInvitationButton invitation={invitation} backend={backend} />

                        <RemoveInvitationButton backend={backend} email={invitation.userEmail} />
                      </Button.Group>
                    )}
                  </div>
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </Scroller>
    </>
  )
}

/** Props for the ResendInvitationButton component. */
interface ResendInvitationButtonProps {
  readonly invitation: backendModule.Invitation
  readonly backend: RemoteBackend
}

/** Button for resending an invitation. */
function ResendInvitationButton(props: ResendInvitationButtonProps) {
  const { invitation, backend } = props

  const { getText } = useText()
  const resendMutation = useMutation(
    backendMutationOptions(backend, 'resendInvitation', {
      mutationKey: [invitation.userEmail],
    }),
  )

  return (
    <Button
      variant="icon"
      size="custom"
      loading={resendMutation.isPending}
      onPress={() => {
        resendMutation.mutate([invitation.userEmail])
      }}
    >
      {getText('resend')}
    </Button>
  )
}

/** Props for a {@link RemoveMemberButton}. */
interface RemoveMemberButtonProps {
  readonly backend: RemoteBackend
  readonly userId: backendModule.UserId
  readonly userEmail: string
  readonly userUsername: string
}

/** Action button for removing a member. */
function RemoveMemberButton(props: RemoveMemberButtonProps) {
  const { backend, userId, userUsername, userEmail } = props
  const { getText } = useText()

  const removeMutation = useMutation(
    backendMutationOptions(backend, 'removeUser', {
      mutationKey: [userId],
      meta: { invalidates: [['listUsers']], awaitInvalidates: true },
    }),
  )

  return (
    <Button
      variant="icon"
      size="custom"
      onPress={() => {
        setModal(
          <ConfirmDeleteModal
            defaultOpen={true}
            cannotUndo={true}
            actionText={getText('deleteUserConfirmation', userUsername, userEmail)}
            alert={getText('deleteUserAlert')}
            onConfirm={async () => {
              await removeMutation.mutateAsync([userId])
            }}
            actionButtonLabel={getText('remove')}
          />,
        )
      }}
    >
      {getText('remove')}
    </Button>
  )
}

/** Props for a {@link RemoveInvitationButton}. */
interface RemoveInvitationButtonProps {
  readonly backend: RemoteBackend
  readonly email: backendModule.EmailAddress
}

/** Action button for removing an invitation. */
function RemoveInvitationButton(props: RemoveInvitationButtonProps) {
  const { backend, email } = props

  const { getText } = useText()

  const removeMutation = useMutation(
    backendMutationOptions(backend, 'deleteInvitation', {
      mutationKey: [email],
      meta: { invalidates: [['listInvitations']], awaitInvalidates: true },
    }),
  )

  return (
    <Button
      variant="icon"
      size="custom"
      loading={removeMutation.isPending}
      onPress={() => removeMutation.mutateAsync([email])}
    >
      {getText('remove')}
    </Button>
  )
}
