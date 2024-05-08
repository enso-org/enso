/** @file Settings tab for viewing and editing organization members. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'
import HorizontalMenuBar from '#/components/styled/HorizontalMenuBar'
import UnstyledButton from '#/components/UnstyledButton'

import InviteUsersModal from '#/modals/InviteUsersModal'

// ==============================
// === MembersSettingsSection ===
// ==============================

/** Settings tab for viewing and editing organization members. */
export default function MembersSettingsSection() {
  const { backend } = backendProvider.useBackend()
  const { getText } = textProvider.useText()
  const { setModal } = modalProvider.useSetModal()
  const membersQuery = reactQuery.useQuery({
    queryKey: ['members'],
    queryFn: () => backend.listUsers(),
  })

  return (
    <>
      <HorizontalMenuBar>
        <UnstyledButton
          className="flex h-row items-center rounded-full bg-frame px-new-project-button-x"
          onPress={() => {
            setModal(<InviteUsersModal eventTarget={null} />)
          }}
        >
          <aria.Text className="text whitespace-nowrap font-semibold">
            {getText('inviteMembers')}
          </aria.Text>
        </UnstyledButton>
      </HorizontalMenuBar>
      <table className="table-fixed self-start rounded-rows">
        <thead>
          <tr className="h-row">
            <th className="w-members-name-column border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0">
              {getText('name')}
            </th>
            <th className="w-members-email-column border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0">
              {getText('email')}
            </th>
          </tr>
        </thead>
        <tbody className="select-text">
          {!membersQuery.isSuccess ? (
            <tr className="h-row">
              <td colSpan={2} className="rounded-full bg-transparent">
                <div className="flex justify-center">
                  <StatelessSpinner size={32} state={statelessSpinner.SpinnerState.loadingMedium} />
                </div>
              </td>
            </tr>
          ) : (
            membersQuery.data.map(member => (
              <tr key={member.userId} className="h-row">
                <td className="text border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0 ">
                  {member.name}
                </td>
                <td className="text border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0 ">
                  {member.email}
                </td>
              </tr>
            ))
          )}
        </tbody>
      </table>
    </>
  )
}
