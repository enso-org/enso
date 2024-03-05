/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'

import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'

import InviteUsersModal from '#/modals/InviteUsersModal'

// ==========================
// === MembersSettingsTab ===
// ==========================

/** Settings tab for viewing and editing organization members. */
export default function MembersSettingsTab() {
  const { backend } = backendProvider.useBackend()
  const { setModal } = modalProvider.useSetModal()
  const members = asyncEffectHooks.useAsyncEffect(null, () => backend.listUsers(), [backend])
  const isLoading = members == null

  return (
    <div className="flex flex-col gap-settings-subsection">
      <div className="flex flex-col gap-settings-section-header">
        <h3 className="settings-subheading">Members</h3>
        <div className="flex gap-drive-bar">
          <button
            className="flex items-center bg-frame rounded-full h-row px-new-project-button-x"
            onClick={event => {
              event.stopPropagation()
              setModal(<InviteUsersModal eventTarget={null} />)
            }}
          >
            <span className="text font-semibold whitespace-nowrap">Invite Members</span>
          </button>
        </div>
        <table className="self-start table-fixed rounded-rows">
          <thead>
            <tr className="h-row">
              <th className="text-left px-cell-x bg-clip-padding border-transparent border-x-2 last:border-r-0 text-sm font-semibold w-members-name-column">
                Name
              </th>
              <th className="text-left px-cell-x bg-clip-padding border-transparent border-x-2 last:border-r-0 text-sm font-semibold w-members-email-column">
                Email
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
                <tr key={member.id} className="h-row">
                  <td className="text bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full border-transparent border-x-2 last:border-r-0 ">
                    {member.name}
                  </td>
                  <td className="text bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full border-transparent border-x-2 last:border-r-0 ">
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
