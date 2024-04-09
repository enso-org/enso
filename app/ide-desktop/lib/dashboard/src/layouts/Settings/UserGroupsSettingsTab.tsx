/** @file Settings tab for viewing and editing roles for all users in the organization. */
import * as React from 'react'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'
import * as refreshHooks from '#/hooks/refreshHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import MembersTable from '#/layouts/Settings/MembersTable'

import * as aria from '#/components/aria'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'
import HorizontalMenuBar from '#/components/styled/HorizontalMenuBar'
import SettingsSection from '#/components/styled/settings/SettingsSection'
import UnstyledButton from '#/components/UnstyledButton'

import NewUserGroupModal from '#/modals/NewUserGroupModal'

// ==============================
// === MemberRolesSettingsTab ===
// ==============================

/** Settings tab for viewing and editing organization members. */
export default function MemberRolesSettingsTab() {
  const { backend } = backendProvider.useBackend()
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const [refresh, doRefresh] = refreshHooks.useRefresh()
  const userGroups = asyncEffectHooks.useAsyncEffect(null, () => backend.listUserGroups(), [
    backend,
    refresh,
  ])
  // NOTE: Neither users nor user groups currently return the information needed to list users
  // within a group.
  const users = asyncEffectHooks.useAsyncEffect(null, () => backend.listUsers(), [backend])
  const isLoading = userGroups == null || users == null

  return (
    <div className="flex h flex-col gap-settings-section lg:h-auto lg:flex-row">
      <div className="flex w-settings-main-section flex-col gap-settings-subsection">
        <SettingsSection noFocusArea title={getText('userGroups')}>
          <HorizontalMenuBar>
            <UnstyledButton
              className="flex h-row items-center rounded-full bg-frame px-new-project-button-x"
              onPress={() => {
                setModal(<NewUserGroupModal onSubmit={doRefresh} />)
              }}
            >
              <aria.Text className="text whitespace-nowrap font-semibold">
                {getText('newUserGroup')}
              </aria.Text>
            </UnstyledButton>
          </HorizontalMenuBar>
          <table className="table-fixed self-start rounded-rows">
            <thead>
              <tr className="h-row">
                <th className="w-members-name-column border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0">
                  {getText('userGroup')}
                </th>
              </tr>
            </thead>
            <tbody className="select-text">
              {isLoading ? (
                <tr className="h-row">
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
                    <tr key={userGroup.groupName} className="h-row">
                      <td className="text border-x-2 border-transparent bg-clip-padding px-cell-x first:rounded-l-full last:rounded-r-full last:border-r-0">
                        {userGroup.groupName}
                      </td>
                    </tr>
                  )
                })
              )}
            </tbody>
          </table>
        </SettingsSection>
      </div>
      <SettingsSection noFocusArea title={getText('users')}>
        <MembersTable />
      </SettingsSection>
    </div>
  )
}
