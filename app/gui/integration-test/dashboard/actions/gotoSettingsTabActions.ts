/** @file Actions for going to a different page. */
import { TEXT } from '.'
import type { PageCallback } from './BaseActions'
import BaseActions from './BaseActions'
import SettingsAccountTabActions from './SettingsAccountTabActions'
import SettingsActivityLogShortcutsTabActions from './SettingsActivityLogTabActions'
import SettingsBillingAndPlansTabActions from './SettingsBillingAndPlansTabActions'
import SettingsKeyboardShortcutsTabActions from './SettingsKeyboardShortcutsTabActions'
import SettingsLocalTabActions from './SettingsLocalTabActions'
import SettingsMembersTabActions from './SettingsMembersTabActions'
import SettingsOrganizationTabActions from './SettingsOrganizationTabActions'
import SettingsUserGroupsTabActions from './SettingsUserGroupsTabActions'

/** Actions for going to a different settings tab. */
export interface GoToSettingsTabActions<Context> {
  readonly account: () => SettingsAccountTabActions<Context>
  readonly organization: () => SettingsOrganizationTabActions<Context>
  readonly local: () => SettingsLocalTabActions<Context>
  readonly billingAndPlans: () => SettingsBillingAndPlansTabActions<Context>
  readonly members: () => SettingsMembersTabActions<Context>
  readonly userGroups: () => SettingsUserGroupsTabActions<Context>
  readonly keyboardShortcuts: () => SettingsKeyboardShortcutsTabActions<Context>
  readonly activityLog: () => SettingsActivityLogShortcutsTabActions<Context>
}

/** Generate actions for going to a different page. */
export function goToSettingsTabActions<Context>(
  step: (name: string, callback: PageCallback<Context>) => BaseActions<Context>,
): GoToSettingsTabActions<Context> {
  return {
    account: () =>
      step('Go to "account" settings tab', (page) =>
        page
          .getByRole('button', { name: TEXT.accountSettingsTab })
          .getByText(TEXT.accountSettingsTab)
          .click(),
      ).into(SettingsAccountTabActions<Context>),
    organization: () =>
      step('Go to "organization" settings tab', (page) =>
        page
          .getByRole('button', { name: TEXT.organizationSettingsTab })
          .getByText(TEXT.organizationSettingsTab)
          .click(),
      ).into(SettingsOrganizationTabActions<Context>),
    local: () =>
      step('Go to "local" settings tab', (page) =>
        page
          .getByRole('button', { name: TEXT.localSettingsTab })
          .getByText(TEXT.localSettingsTab)
          .click(),
      ).into(SettingsLocalTabActions<Context>),
    billingAndPlans: () =>
      step('Go to "billing and plans" settings tab', (page) =>
        page
          .getByRole('button', { name: TEXT.billingAndPlansSettingsTab })
          .getByText(TEXT.billingAndPlansSettingsTab)
          .click(),
      ).into(SettingsBillingAndPlansTabActions<Context>),
    members: () =>
      step('Go to "members" settings tab', (page) =>
        page
          .getByRole('button', { name: TEXT.membersSettingsTab })
          .getByText(TEXT.membersSettingsTab)
          .click(),
      ).into(SettingsMembersTabActions<Context>),
    userGroups: () =>
      step('Go to "user groups" settings tab', (page) =>
        page
          .getByRole('button', { name: TEXT.userGroupsSettingsTab })
          .getByText(TEXT.userGroupsSettingsTab)
          .click(),
      ).into(SettingsUserGroupsTabActions<Context>),
    keyboardShortcuts: () =>
      step('Go to "keyboard shortcuts" settings tab', (page) =>
        page
          .getByRole('button', { name: TEXT.keyboardShortcutsSettingsTab })
          .getByText(TEXT.keyboardShortcutsSettingsTab)
          .click(),
      ).into(SettingsKeyboardShortcutsTabActions<Context>),
    activityLog: () =>
      step('Go to "activity log" settings tab', (page) =>
        page
          .getByRole('button', { name: TEXT.activityLogSettingsTab })
          .getByText(TEXT.activityLogSettingsTab)
          .click(),
      ).into(SettingsActivityLogShortcutsTabActions<Context>),
  }
}
