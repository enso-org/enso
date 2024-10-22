/** @file Settings tab for viewing and editing organization members. */
import { useMutation, useSuspenseQueries } from '@tanstack/react-query'

import { backendMutationOptions } from '#/hooks/backendHooks'
import * as billingHooks from '#/hooks/billing'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import * as paywall from '#/components/Paywall'

import InviteUsersModal from '#/modals/InviteUsersModal'

import type * as backendModule from '#/services/Backend'
import type RemoteBackend from '#/services/RemoteBackend'

// =================
// === Constants ===
// =================

const LIST_USERS_STALE_TIME_MS = 60_000

// ==============================
// === MembersSettingsSection ===
// ==============================

/** Settings tab for viewing and editing organization members. */
export default function MembersSettingsSection() {
  const { getText } = textProvider.useText()
  const backend = backendProvider.useRemoteBackend()
  const { user } = authProvider.useFullUserSession()

  const { isFeatureUnderPaywall, getFeature } = billingHooks.usePaywall({ plan: user.plan })

  const [{ data: members }, { data: invitations }] = useSuspenseQueries({
    queries: [
      {
        queryKey: ['listUsers'],
        queryFn: () => backend.listUsers(),
        staleTime: LIST_USERS_STALE_TIME_MS,
      },

      {
        queryKey: ['listInvitations'],
        queryFn: () => backend.listInvitations(),
        staleTime: LIST_USERS_STALE_TIME_MS,
      },
    ],
  })

  const isUnderPaywall = isFeatureUnderPaywall('inviteUserFull')
  const feature = getFeature('inviteUser')

  const seatsLeft = isUnderPaywall ? invitations.availableLicenses : null
  const seatsTotal = feature.meta.maxSeats
  const isAdmin = user.isOrganizationAdmin

  return (
    <>
      {isAdmin && (
        <ariaComponents.ButtonGroup>
          <ariaComponents.DialogTrigger>
            <ariaComponents.Button variant="outline" rounded="full" size="medium">
              {getText('inviteMembers')}
            </ariaComponents.Button>

            <InviteUsersModal />
          </ariaComponents.DialogTrigger>

          {seatsLeft != null && (
            <div className="flex items-center gap-1">
              <ariaComponents.Text>
                {seatsLeft <= 0 ?
                  getText('noSeatsLeft')
                : getText('seatsLeft', seatsLeft, seatsTotal)}
              </ariaComponents.Text>

              <paywall.PaywallDialogButton
                feature="inviteUserFull"
                variant="link"
                showIcon={false}
              />
            </div>
          )}
        </ariaComponents.ButtonGroup>
      )}

      <table className="table-fixed self-start rounded-rows">
        <thead>
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
            <tr key={member.email} className="group h-row rounded-rows-child">
              <td className="min-w-48 max-w-80 border-x-2 border-transparent bg-clip-padding px-4 py-1 first:rounded-l-full last:rounded-r-full last:border-r-0">
                <ariaComponents.Text truncate="1" className="block">
                  {member.email}
                </ariaComponents.Text>
                <ariaComponents.Text truncate="1" className="block text-2xs text-primary/40">
                  {member.name}
                </ariaComponents.Text>
              </td>
              <td className="border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
                <div className="flex flex-col">
                  {getText('active')}
                  {member.email !== user.email && isAdmin && (
                    <ariaComponents.ButtonGroup gap="small" className="mt-0.5">
                      <RemoveMemberButton backend={backend} userId={member.userId} />
                    </ariaComponents.ButtonGroup>
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
                    <ariaComponents.ButtonGroup gap="small" className="mt-0.5">
                      <ariaComponents.CopyButton
                        size="custom"
                        // eslint-disable-next-line @typescript-eslint/naming-convention, camelcase
                        copyText={`enso://auth/registration?=${new URLSearchParams({ organization_id: invitation.organizationId }).toString()}`}
                        aria-label={getText('copyInviteLink')}
                        copyIcon={false}
                      >
                        {getText('copyInviteLink')}
                      </ariaComponents.CopyButton>

                      <ResendInvitationButton invitation={invitation} backend={backend} />

                      <RemoveInvitationButton backend={backend} email={invitation.userEmail} />
                    </ariaComponents.ButtonGroup>
                  )}
                </div>
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </>
  )
}

// ==============================
// === ResendInvitationButton ===
// ==============================

/** Props for the ResendInvitationButton component. */
interface ResendInvitationButtonProps {
  readonly invitation: backendModule.Invitation
  readonly backend: RemoteBackend
}

/** Button for resending an invitation. */
function ResendInvitationButton(props: ResendInvitationButtonProps) {
  const { invitation, backend } = props

  const { getText } = textProvider.useText()
  const resendMutation = useMutation(
    backendMutationOptions(backend, 'resendInvitation', {
      mutationKey: [invitation.userEmail],
    }),
  )

  return (
    <ariaComponents.Button
      variant="icon"
      size="custom"
      loading={resendMutation.isPending}
      onPress={() => {
        resendMutation.mutate([invitation.userEmail])
      }}
    >
      {getText('resend')}
    </ariaComponents.Button>
  )
}

// ==========================
// === RemoveMemberButton ===
// ==========================

/** Props for a {@link RemoveMemberButton}. */
interface RemoveMemberButtonProps {
  readonly backend: RemoteBackend
  readonly userId: backendModule.UserId
}

/** Action button for removing a member. */
function RemoveMemberButton(props: RemoveMemberButtonProps) {
  const { backend, userId } = props
  const { getText } = textProvider.useText()

  const removeMutation = useMutation(
    backendMutationOptions(backend, 'removeUser', {
      mutationKey: [userId],
      meta: { invalidates: [['listUsers']], awaitInvalidates: true },
    }),
  )

  return (
    <ariaComponents.Button
      variant="icon"
      size="custom"
      onPress={() => removeMutation.mutateAsync([userId])}
    >
      {getText('remove')}
    </ariaComponents.Button>
  )
}

// ==============================
// === RemoveInvitationButton ===
// ==============================

/** Props for a {@link RemoveInvitationButton}. */
interface RemoveInvitationButtonProps {
  readonly backend: RemoteBackend
  readonly email: backendModule.EmailAddress
}

/** Action button for removing an invitation. */
function RemoveInvitationButton(props: RemoveInvitationButtonProps) {
  const { backend, email } = props

  const { getText } = textProvider.useText()

  const removeMutation = useMutation(
    backendMutationOptions(backend, 'deleteInvitation', {
      mutationKey: [email],
      meta: { invalidates: [['listInvitations']], awaitInvalidates: true },
    }),
  )

  return (
    <ariaComponents.Button
      variant="icon"
      size="custom"
      loading={removeMutation.isPending}
      onPress={() => removeMutation.mutateAsync([email])}
    >
      {getText('remove')}
    </ariaComponents.Button>
  )
}
