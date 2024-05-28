/** @file Settings tab for viewing and editing organization members. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as textProvider from '#/providers/TextProvider'

import MembersSettingsTabBar from '#/layouts/Settings/MembersSettingsTabBar'

import * as ariaComponents from '#/components/AriaComponents'
import SettingsPage from '#/components/styled/settings/SettingsPage'
import SettingsSection from '#/components/styled/settings/SettingsSection'

import type * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

// =================
// === Constants ===
// =================

const LIST_USERS_STALE_TIME_MS = 60_000

// ==========================
// === MembersSettingsTab ===
// ==========================

/** Props for a {@link MembersSettingsTab}. */
export interface MembersSettingsTabProps {
  readonly backend: Backend
}

/** Settings tab for viewing and editing organization members. */
export default function MembersSettingsTab(props: MembersSettingsTabProps) {
  const { backend } = props
  const { getText } = textProvider.useText()

  const [{ data: members }, { data: invitations }] = reactQuery.useSuspenseQueries({
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

  return (
    <SettingsPage>
      <SettingsSection noFocusArea title={getText('members')} className="overflow-hidden">
        <MembersSettingsTabBar />

        <table className="table-fixed self-start rounded-rows">
          <thead>
            <tr className="h-row">
              <th className="w-members-name-column border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0">
                {getText('name')}
              </th>
              <th className="w-members-email-column border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0">
                {getText('status')}
              </th>
            </tr>
          </thead>
          <tbody className="select-text">
            {members.map(member => (
              <tr key={member.email} className="group h-row rounded-rows-child">
                <td className="border-x-2 border-transparent bg-clip-padding px-4 py-1 first:rounded-l-full last:rounded-r-full last:border-r-0">
                  <span className="block text-sm">{member.email}</span>
                  <span className="block text-xs text-primary/50">{member.name}</span>
                </td>
                <td className="border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
                  <div className="flex flex-col">
                    {getText('active')}
                    <ariaComponents.ButtonGroup gap="small" className="mt-0.5">
                      <RemoveMemberButton backend={backend} email={member.email} />
                    </ariaComponents.ButtonGroup>
                  </div>
                </td>
              </tr>
            ))}

            {invitations.map(invitation => (
              <tr key={invitation.userEmail} className="group h-row rounded-rows-child">
                <td className="border-x-2 border-transparent bg-clip-padding px-4 py-1 first:rounded-l-full last:rounded-r-full last:border-r-0">
                  <span className="block text-sm">{invitation.userEmail}</span>
                </td>
                <td className="border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
                  <div className="flex flex-col">
                    {getText('pendingInvitation')}
                    <ariaComponents.ButtonGroup gap="small" className="mt-0.5">
                      <ariaComponents.CopyButton
                        size="custom"
                        copyText={`enso://auth/registration?organization_id=${invitation.organizationId}`}
                        aria-label={getText('copyInviteLink')}
                        copyIcon={false}
                      >
                        {getText('copyInviteLink')}
                      </ariaComponents.CopyButton>

                      <ResendInvitationButton invitation={invitation} backend={backend} />

                      <RemoveInvitationButton backend={backend} email={invitation.userEmail} />
                    </ariaComponents.ButtonGroup>
                  </div>
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </SettingsSection>
    </SettingsPage>
  )
}

// ==============================
// === ResendInvitationButton ===
// ==============================

/** Props for the ResendInvitationButton component. */
interface ResendInvitationButtonProps {
  readonly invitation: backendModule.Invitation
  readonly backend: Backend
}

/** Button for resending an invitation. */
function ResendInvitationButton(props: ResendInvitationButtonProps) {
  const { invitation, backend } = props

  const { getText } = textProvider.useText()
  const resendMutation = reactQuery.useMutation({
    mutationKey: ['resendInvitation', invitation.userEmail],
    mutationFn: (email: backendModule.EmailAddress) => backend.resendInvitation(email),
  })

  return (
    <ariaComponents.Button
      variant="icon"
      size="custom"
      loading={resendMutation.isPending}
      onPress={() => {
        resendMutation.mutate(invitation.userEmail)
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
  readonly email: backendModule.EmailAddress
  readonly backend: Backend
}

/** Action button for removing a member. */
function RemoveMemberButton(props: RemoveMemberButtonProps) {
  const { email } = props
  const { getText } = textProvider.useText()

  const queryClient = reactQuery.useQueryClient()

  const removeMutation = reactQuery.useMutation({
    mutationKey: ['removeUser', email],
    mutationFn: async () => {
      // TODO: Implement remove member mutation
    },
    onSuccess: () =>
      queryClient.invalidateQueries({
        queryKey: ['listUsers'],
      }),
  })

  return (
    <ariaComponents.Button
      variant="icon"
      size="custom"
      onPress={() => removeMutation.mutateAsync()}
    >
      {getText('remove')}
    </ariaComponents.Button>
  )
}

// ==============================
// === RemoveInvitationButton ===
// ==============================

/** Action button for removing an invitation. */
function RemoveInvitationButton(props: RemoveMemberButtonProps) {
  const { backend, email } = props

  const { getText } = textProvider.useText()
  const queryClient = reactQuery.useQueryClient()

  const removeMutation = reactQuery.useMutation({
    mutationKey: ['resendInvitation', email],
    mutationFn: () => backend.deleteInvitation(email),
    onSuccess: () =>
      queryClient.invalidateQueries({
        queryKey: ['listInvitations'],
      }),
  })

  return (
    <ariaComponents.Button
      variant="icon"
      size="custom"
      loading={removeMutation.isPending}
      onPress={() => {
        removeMutation.mutate()
      }}
    >
      {getText('remove')}
    </ariaComponents.Button>
  )
}
