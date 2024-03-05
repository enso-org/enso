/** @file Settings tab for viewing and editing roles for all users in the organization. */
import * as React from 'react'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'

import * as backendProvider from '#/providers/BackendProvider'

import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'

// ==============================
// === MemberRolesSettingsTab ===
// ==============================

/** Settings tab for viewing and editing organization members. */
export default function MemberRolesSettingsTab() {
  const { backend } = backendProvider.useBackend()
  const userGroups = asyncEffectHooks.useAsyncEffect(null, () => backend.listUserGroups(), [
    backend,
  ])
  const isLoading = userGroups == null

  return (
    <div className="flex flex-col gap-8">
      <div className="flex flex-col gap-2.5">
        <h3 className="font-bold text-xl h-9.5 py-0.5">Member Roles</h3>
        <table className="rounded-rows self-start table-fixed">
          <thead>
            <tr className="h-8">
              <th className="text-left bg-clip-padding border-transparent border-l-2 border-r-2 text-sm font-semibold w-32">
                Role
              </th>
            </tr>
          </thead>
          <tbody className="select-text">
            {isLoading ? (
              <tr className="h-8">
                <td colSpan={3} className="bg-transparent">
                  <div className="flex justify-center">
                    <StatelessSpinner
                      size={32}
                      state={statelessSpinner.SpinnerState.loadingMedium}
                    />
                  </div>
                </td>
              </tr>
            ) : (
              userGroups.map(userGroup => {
                return (
                  <tr key={userGroup.groupName} className="h-8">
                    <td className="rounded-l-full">{userGroup.groupName}</td>
                  </tr>
                )
              })
            )}
          </tbody>
        </table>
      </div>
    </div>
  )
}
