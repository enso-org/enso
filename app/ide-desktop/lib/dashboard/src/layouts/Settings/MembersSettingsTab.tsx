/** @file Settings tab for viewing and editing all users in the organization. */
import * as React from 'react'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'

import InviteUsersModal from '#/modals/InviteUsersModal'

// ==========================
// === MembersSettingsTab ===
// ==========================

/** Settings tab for viewing and editing organization members. */
export default function MembersSettingsTab() {
  const { backend } = backendProvider.useBackend()
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const members = asyncEffectHooks.useAsyncEffect(null, () => backend.listUsers(), [backend])
  const isLoading = members == null

  return (
    <div className="flex flex-col gap-settings-subsection">
      <div className="flex flex-col gap-settings-section-header">
        <h3 className="settings-subheading">{getText('members')}</h3>
        <div className="flex gap-drive-bar">
          <button
            className="flex h-row items-center rounded-full bg-frame px-new-project-button-x"
            onClick={event => {
              event.stopPropagation()
              setModal(<InviteUsersModal eventTarget={null} />)
            }}
          >
            <span className="text whitespace-nowrap font-semibold">{getText('inviteMembers')}</span>
          </button>
        </div>
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
            {isLoading ? (
              <tr className="h-row">
                <td colSpan={2} className="rounded-full">
                  <div className="flex justify-center">
                    <StatelessSpinner
                      size={32}
                      state={statelessSpinner.SpinnerState.loadingMedium}
                    />
                  </div>
                </td>
              </tr>
            ) : (
              members.map(member => (
                <tr key={member.userId} className="h-row">
                  <td className="text border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
                    {member.name}
                  </td>
                  <td className="text border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
                    {member.email}
                  </td>
                </tr>
              ))
            )}
          </tbody>
        </table>
      </div>
    </div>
  )
}
