/** @file Test the user settings tab. */
import { test } from '@playwright/test'

import { mockAllAndLogin } from './actions'

const DATA_LINK_NAME = 'a data link'

test('data link editor', ({ page }) =>
  mockAllAndLogin({ page })
    .openDataLinkModal()
    .withNameInput(async (input) => {
      await input.fill(DATA_LINK_NAME)
    }))
