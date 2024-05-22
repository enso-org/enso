/** @file Tests for the asset panel. */
import * as test from '@playwright/test'

import * as backend from '#/services/Backend'

import * as permissions from '#/utilities/permissions'

import * as actions from './actions'

// =================
// === Constants ===
// =================

/** An example description for the asset selected in the asset panel. */
const DESCRIPTION = 'foo bar'
/** An example owner username for the asset selected in the asset panel. */
const USERNAME = 'baz quux'
/** An example owner email for the asset selected in the asset panel. */
const EMAIL = 'baz.quux@email.com'

// =============
// === Tests ===
// =============

test.test('open and close asset panel', ({ page }) =>
  actions.mockAllAndLogin({ page }).then(
    async ({ pageActions }) =>
      await pageActions
        .createFolder()
        .driveTable.clickRow(0)
        .withAssetPanel(async assetPanel => {
          await test.expect(assetPanel).not.toBeVisible()
        })
        .toggleAssetPanel()
        .withAssetPanel(async assetPanel => {
          await test.expect(assetPanel).toBeVisible()
        })
        .toggleAssetPanel()
        .withAssetPanel(async assetPanel => {
          await test.expect(assetPanel).not.toBeVisible()
        })
  )
)

test.test('asset panel contents', ({ page }) =>
  actions.mockAll({ page }).then(
    async ({ pageActions, api }) =>
      await pageActions
        .do(() => {
          const { defaultOrganizationId, defaultUserId } = api
          api.addProject('project', {
            description: DESCRIPTION,
            permissions: [
              {
                permission: permissions.PermissionAction.own,
                user: {
                  organizationId: defaultOrganizationId,
                  userId: defaultUserId,
                  name: USERNAME,
                  email: backend.EmailAddress(EMAIL),
                },
              },
            ],
          })
        })
        .login()
        .driveTable.clickRow(0)
        .toggleAssetPanel()
        .do(async () => {
          await test.expect(actions.locateAssetPanelDescription(page)).toHaveText(DESCRIPTION)
          // `getByText` is required so that this assertion works if there are multiple permissions.
          await test
            .expect(actions.locateAssetPanelPermissions(page).getByText(USERNAME))
            .toBeVisible()
        })
  )
)
