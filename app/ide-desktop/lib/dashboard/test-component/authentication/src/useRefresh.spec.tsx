/** @file Tests for `error.ts`. */
import * as React from 'react'

import * as test from '@playwright/experimental-ct-react'

import Refresh, * as refresh from './useRefresh/refresh'

/* eslint-disable @typescript-eslint/no-magic-numbers */

test.test('useRefresh', async ({ mount }) => {
    const values = new Set<refresh.RefreshState>()
    const onRefresh = (refreshState: refresh.RefreshState) => {
        values.add(refreshState)
    }
    const component = await mount(<Refresh onRefresh={onRefresh} />)
    test.expect(values.size).toBe(0)
    await new Promise(resolve => setTimeout(resolve, 20))
    test.expect(values.size).toBe(1)
    await component.click()
    test.expect(values.size, '`onRefresh` is triggered when `doRefresh` is called').toBe(2)
    await component.click()
    test.expect(values.size, '`refresh` states are all unique').toBe(3)
    await component.click()
    test.expect(values.size, '`refresh` states are all unique').toBe(4)
    await component.click()
    test.expect(values.size, '`refresh` states are all unique').toBe(5)
})
