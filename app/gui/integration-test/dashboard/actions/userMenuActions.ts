/** @file Actions for the user menu. */
import type { Download } from '@playwright/test'

import { TEXT } from '.'
import type BaseActions from './BaseActions'
import type { PageCallback } from './BaseActions'
import LoginPageActions from './LoginPageActions'
import SettingsPageActions from './SettingsPageActions'

/** Actions for the user menu. */
export interface UserMenuActions<T extends BaseActions<Context>, Context> {
  readonly downloadApp: (callback: (download: Download) => Promise<void> | void) => T
  readonly settings: () => SettingsPageActions<Context>
  readonly logout: () => LoginPageActions<Context>
  readonly goToLoginPage: () => LoginPageActions<Context>
}

/** Generate actions for the user menu. */
export function userMenuActions<T extends BaseActions<Context>, Context>(
  step: (name: string, callback: PageCallback<Context>) => T,
): UserMenuActions<T, Context> {
  return {
    downloadApp: (callback: (download: Download) => Promise<void> | void) =>
      step('Download app (user menu)', async (page) => {
        const downloadPromise = page.waitForEvent('download')
        await page
          .getByRole('button', { name: TEXT.downloadAppShortcut })
          .getByText(TEXT.downloadAppShortcut)
          .click()
        await callback(await downloadPromise)
      }),
    settings: () =>
      step('Go to Settings (user menu)', async (page) => {
        await page
          .getByRole('button', { name: TEXT.settingsShortcut })
          .getByText(TEXT.settingsShortcut)
          .click()
      }).into(SettingsPageActions<Context>),
    logout: () =>
      step('Logout (user menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.signOutShortcut })
          .getByText(TEXT.signOutShortcut)
          .click(),
      ).into(LoginPageActions<Context>),
    goToLoginPage: () =>
      step('Login (user menu)', (page) =>
        page
          .getByRole('button', { name: TEXT.signInShortcut, exact: true })
          .getByText(TEXT.signInShortcut)
          .click(),
      ).into(LoginPageActions<Context>),
  }
}
