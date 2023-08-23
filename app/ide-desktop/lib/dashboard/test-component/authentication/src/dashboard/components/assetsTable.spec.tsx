/** @file Tests for `error.ts`. */
// import * as React from 'react'
//
// import * as test from '@playwright/experimental-ct-react'
//
// import * as assetListEventModule from '../../../../../src/authentication/src/dashboard/events/assetListEvent'
// import * as backendProvider from '../../../../../src/authentication/src/providers/backend'
// import * as remoteBackend from '../../../../../mock/authentication/src/dashboard/remoteBackend'
//
// import AssetsTable from '../../../../../src/authentication/src/dashboard/components/assetsTable'

/* eslint-disable @typescript-eslint/no-magic-numbers */

// test.test('assetsTable', async ({ mount }) => {
//     const backend = new remoteBackend.RemoteBackend()
//     const component = await mount(
//         <backendProvider.BackendProvider initialBackend={backend}>
//             <AssetsTable
//                 appRunner={{
//                     runApp: async () => {
//                         // Ignored.
//                     },
//                     stopApp: () => {
//                         // Ignored.
//                     },
//                 }}
//             />
//         </backendProvider.BackendProvider>
//     )
//     test.expect(values.size).toBe(0)
//     await new Promise(resolve => setTimeout(resolve, 20))
//     test.expect(values.size).toBe(1)
//     await component.click()
//     test.expect(values.size, '`onRefresh` is triggered when `doRefresh` is called').toBe(2)
//     await component.click()
//     test.expect(values.size, '`refresh` states are all unique').toBe(3)
//     await component.click()
//     test.expect(values.size, '`refresh` states are all unique').toBe(4)
//     await component.click()
//     test.expect(values.size, '`refresh` states are all unique').toBe(5)
// })
