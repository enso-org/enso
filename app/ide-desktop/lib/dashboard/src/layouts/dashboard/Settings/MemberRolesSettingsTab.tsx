/** @file Settings tab for viewing and editing roles for all users in the organization. */
import * as React from 'react'

import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'

import * as backendProvider from '#/providers/BackendProvider'

import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'

// ==============================
// === MemberRolesSettingsTab ===
// ==============================

/** Settings tab for viewing and editing organization members. */
export default function MemberRolesSettingsTab() {
  const { backend } = backendProvider.useBackend()
  const roles = asyncEffectHooks.useAsyncEffect(null, () => backend.listRoles(), [backend])
  const isLoading = roles == null

  return (
    <div className="flex flex-col gap-8">
      <div className="flex flex-col gap-2.5">
        <h3 className="font-bold text-xl h-9.5 py-0.5">Member Roles</h3>
        <table className="self-start table-fixed">
          <thead>
            <tr className="h-8">
              <th className="text-left bg-clip-padding border-transparent border-l-2 border-r-2 text-sm font-semibold w-32">
                Role
              </th>
              <th className="text-left bg-clip-padding border-transparent border-l-2 border-r-2 text-sm font-semibold w-33.25">
                Created at
              </th>
              <th className="text-left bg-clip-padding border-transparent border-l-2 text-sm font-semibold w-48">
                Users
              </th>
            </tr>
          </thead>
          <tbody className="select-text">
            {isLoading ? (
              <tr className="h-8">
                <td colSpan={2}>
                  <div className="flex justify-center">
                    <StatelessSpinner
                      size={32}
                      state={statelessSpinner.SpinnerState.loadingMedium}
                    />
                  </div>
                </td>
              </tr>
            ) : (
              roles.roles.map(role => {
                return (
                  <tr key={role.name} className="h-8">
                    <td>{role.name}</td>
                    <td>{role.createdAt}</td>
                    <td>
                      <div className="flex gap-1">
                        {role.users.map(user => (
                          <div
                            key={user.id}
                            className="flex items-center rounded-full overflow-clip w-7.25 h-7.25"
                          >
                            <img src={user.picture ?? DefaultUserIcon} />
                          </div>
                        ))}
                      </div>
                    </td>
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
