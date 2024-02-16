/** @file Settings tab for viewing and editing roles for all users in the organization. */
import * as React from 'react'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'

import * as backendProvider from '#/providers/BackendProvider'

import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'

import type * as backendModule from '#/services/Backend'

// ==============================
// === MemberRolesSettingsTab ===
// ==============================

const NO_ROLES_SYMBOL = Symbol('no roles')

/** Settings tab for viewing and editing organization members. */
export default function MemberRolesSettingsTab() {
  const { backend } = backendProvider.useBackend()
  const members = asyncEffectHooks.useAsyncEffect(null, () => backend.listUsers(), [backend])
  const roles = React.useMemo(() => {
    const rolesMap = new Map<string | typeof NO_ROLES_SYMBOL, backendModule.SimpleUser[]>()
    const membersWithoutRoles: backendModule.SimpleUser[] = []
    rolesMap.set(NO_ROLES_SYMBOL, membersWithoutRoles)
    for (const member of members ?? []) {
      for (const group of member.roles ?? []) {
        let membersInGroup = rolesMap.get(group)
        if (membersInGroup == null) {
          membersInGroup = []
          rolesMap.set(group, membersInGroup)
        }
        membersInGroup.push(member)
      }
      if (member.roles == null || member.roles.length === 0) {
        membersWithoutRoles.push(member)
      }
    }
    return rolesMap
  }, [members])
  const isLoading = members == null

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
                              Array.from(roles.entries(), kv => { 
                              const [role, members] = kv
                return <tr key={role} className="h-8">
                                  <td>{role.name}</td>
                                  <td>{members.map(member => member.picture)}</td>
                              </tr>
                          }
            )}
          </tbody>
        </table>
      </div>
    </div>
  )
}
