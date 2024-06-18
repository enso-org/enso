/** @file Tests for the asset panel. */
import * as test from '@playwright/test'

import * as backend from '#/services/Backend'

import * as permissions from '#/utilities/permissions'

import * as actions from './actions'

test.test('open and close asset panel', async ({ page }) => {
  await actions.mockAllAndLogin({ page })
  const assetRows = actions.locateAssetRows(page)

  await actions.locateNewFolderIcon(page).click()
  await actions.clickAssetRow(assetRows.nth(0))
  await test.expect(actions.locateAssetPanel(page)).not.toBeVisible()
  await actions.locateAssetPanelIcon(page).click()
  await test.expect(actions.locateAssetPanel(page)).toBeVisible()
  await actions.locateAssetPanelIcon(page).click()
  await test.expect(actions.locateAssetPanel(page)).not.toBeVisible()
})

test.test('asset panel contents', async ({ page }) => {
  const { api } = await actions.mockAll({ page })
  const { defaultOrganizationId, defaultUserId } = api
  const assetRows = actions.locateAssetRows(page)
  const description = 'foo bar'
  const username = 'baz quux'
  const email = 'baz.quux@email.com'
  api.addProject('project', {
    description,
    permissions: [
      {
        permission: permissions.PermissionAction.own,
        user: {
          organizationId: defaultOrganizationId,
          userId: defaultUserId,
          name: username,
          email: backend.EmailAddress(email),
        },
      },
    ],
  })
  await page.goto('/')
  await actions.login({ page })

  await actions.clickAssetRow(assetRows.nth(0))
  await actions.locateAssetPanelIcon(page).click()
  await test.expect(actions.locateAssetPanelDescription(page)).toHaveText(description)
  // `getByText` is required so that this assertion works if there are multiple permissions.
  await test.expect(actions.locateAssetPanelPermissions(page).getByText(username)).toBeVisible()
})
