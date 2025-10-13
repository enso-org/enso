/** @file Actions for the user menu. */
import type { Download } from 'integration-test/base'
import type BaseActions from './BaseActions'
import type { BaseActionsClass, PageCallback } from './BaseActions'
import LoginPageActions from './LoginPageActions'
import SettingsPageActions from './SettingsPageActions'
import { TEXT } from './utilities'

/** Actions for the user menu. */
export interface UserMenuActions<
  T extends BaseActions<Context, ParentClass>,
  Context,
  ParentClass extends BaseActionsClass<Context> = never,
> {
  readonly downloadApp: (callback: (download: Download) => Promise<void> | void) => T
  readonly settings: () => SettingsPageActions<Context>
  readonly logout: () => LoginPageActions<Context>
}

/** Generate actions for the user menu. */
export function userMenuActions<
  Context,
  T extends BaseActions<Context, ParentClass>,
  ParentClass extends BaseActionsClass<Context>,
>(
  step: (name: string, callback: PageCallback<Context, T>) => T,
): UserMenuActions<T, Context, ParentClass> {
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
  }
}
