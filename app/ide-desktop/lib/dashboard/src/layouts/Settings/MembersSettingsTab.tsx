/** @file Settings tab for viewing and editing organization members. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as textProvider from '#/providers/TextProvider'

import MembersSettingsTabBar from '#/layouts/Settings/MembersSettingsTabBar'
import MembersTable from '#/layouts/Settings/MembersTable'

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
        <MembersSettingsTabBar backend={backend} />
        <MembersTable allowDelete backend={backend} />
      </SettingsSection>
    </SettingsPage>
  )
}

/**
 * Props for the ResendInvitationButton component.
 */
interface ResendInvitationButtonProps {
  readonly invitation: backendModule.Invitation
  readonly backend: Backend
}

/**
 * Button for resending an invitation.
 */
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

/**
 * Props for the RemoveMemberButton component.
 */
interface RemoveMemberButtonProps {
  readonly email: backendModule.EmailAddress
  readonly backend: Backend
}

/**
 * Action button for removing a member.
 */
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

/**
 * Action button for removing an invitation.
 */
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
