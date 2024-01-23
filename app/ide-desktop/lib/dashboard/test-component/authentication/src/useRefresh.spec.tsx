/** @file Tests for the `useRefresh` hook. */
import * as React from 'react'

import * as test from '@playwright/experimental-ct-react'

import type * as refresh from './useRefresh/refresh'
import Refresh from './useRefresh/refresh'

test.test('useRefresh', async ({ mount }) => {
  const values = new Set<refresh.RefreshState>()
  const onRefresh = (refreshState: refresh.RefreshState) => {
    values.add(refreshState)
  }
  const component = await mount(<Refresh onRefresh={onRefresh} />)
  test.expect(values.size).toBe(0)
  await component.waitFor({ state: 'attached' })
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
