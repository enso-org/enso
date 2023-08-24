/** @file Tests for the `AssetsTable` component. */
// import * as React from 'react'

// import * as test from '@playwright/experimental-ct-react'
// import * as testBase from '@playwright/test'

// import * as api from '../../../../../test-e2e/api'

// import DriveViewWrapper from './driveView/driveView'

// // ================
// // === Locators ===
// // ================

// /** Find an assets table (if any) on the current page. */
// function locateAssetsRows(page: testBase.Locator) {
//     // There is no simple alternative.
//     // eslint-disable-next-line no-restricted-properties
//     return page.locator('table > tbody > tr')
// }

// /** Find an "new project" button (if any) on the current page. */
// function locateNewProjectButton(page: testBase.Locator) {
//     return page.getByRole('button', { name: 'New Project' })
// }

// // =============
// // === Tests ===
// // =============

// test.test('driveView', async ({ page, mount }) => {
//     await api.mockApi(page)
//     const component = await mount(<DriveViewWrapper />)

//     await test.expect(locateAssetsRows(component)).toHaveCount(0)
//     await locateNewProjectButton(component).click()
//     await test.expect(locateAssetsRows(component)).toHaveCount(1)
// })
