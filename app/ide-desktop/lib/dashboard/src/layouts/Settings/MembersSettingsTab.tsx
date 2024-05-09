/** @file Settings tab for viewing and editing organization members. */
import * as React from 'react'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'

import * as textProvider from '#/providers/TextProvider'

import MembersSettingsTabBar from '#/layouts/Settings/MembersSettingsTabBar'

import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'
import SettingsPage from '#/components/styled/settings/SettingsPage'
import SettingsSection from '#/components/styled/settings/SettingsSection'

import type Backend from '#/services/Backend'

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
  const members = asyncEffectHooks.useAsyncEffect(null, () => backend.listUsers(), [backend])
  const isLoading = members == null

  return (
    <SettingsPage>
      <SettingsSection noFocusArea title={getText('members')}>
        <MembersSettingsTabBar backend={backend} />
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
                <td colSpan={2} className="rounded-full bg-transparent">
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
      </SettingsSection>
    </SettingsPage>
  )
}
