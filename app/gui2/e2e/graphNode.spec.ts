import { expect, test } from '@playwright/test'

test('node can open visualization', async ({ page }) => {
  await page.goto('/')
})
