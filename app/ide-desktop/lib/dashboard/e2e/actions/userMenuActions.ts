/** @file Actions for the user menu. */
import type * as test from 'playwright/test'

import type * as baseActions from './BaseActions'
import type BaseActions from './BaseActions'
import LoginPageActions from './LoginPageActions'
import SettingsModalActions from './SettingsModalActions'

// =======================
// === UserMenuActions ===
// =======================

/** Actions for the user menu. */
export interface UserMenuActions<
  T extends BaseActions,
  PreviousStateClass extends new (
    page: test.Page,
    promise?: Promise<void>
  ) => InstanceType<PreviousStateClass>,
> {
  readonly downloadApp: (callback: (download: test.Download) => Promise<void> | void) => T
  readonly openSettingsModal: () => SettingsModalActions<PreviousStateClass>
  readonly logout: () => LoginPageActions
  readonly goToLoginPage: () => LoginPageActions
}

// =======================
// === userMenuActions ===
// =======================

/** Generate actions for the user menu. */
export function userMenuActions<
  T extends BaseActions,
  PreviousStateClass extends new (
    page: test.Page,
    promise?: Promise<void>
  ) => InstanceType<PreviousStateClass>,
>(
  step: (name: string, callback: baseActions.PageCallback) => T,
  previousStateClass: PreviousStateClass
): UserMenuActions<T, PreviousStateClass> {
  return {
    downloadApp: (callback: (download: test.Download) => Promise<void> | void) => {
      return step('Download app (user menu)', async page => {
        const downloadPromise = page.waitForEvent('download')
        await page.getByRole('button', { name: 'Download App' }).getByText('Download App').click()
        await callback(await downloadPromise)
      })
    },
    openSettingsModal: () =>
      step('Go to Settings (user menu)', page =>
        page.getByRole('button', { name: 'Settings' }).getByText('Settings').click()
      ).into(SettingsModalActions<PreviousStateClass>, previousStateClass),
    logout: () =>
      step('Logout (user menu)', page =>
        page.getByRole('button', { name: 'Logout' }).getByText('Logout').click()
      ).into(LoginPageActions),
    goToLoginPage: () =>
      step('Login (user menu)', page =>
        page.getByRole('button', { name: 'Login', exact: true }).getByText('Login').click()
      ).into(LoginPageActions),
  }
}
