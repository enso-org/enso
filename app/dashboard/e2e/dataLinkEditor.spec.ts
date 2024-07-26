/** @file Test the user settings tab. */
import * as test from '@playwright/test'

import * as actions from './actions'

const DATA_LINK_NAME = 'a data link'

test.test('data link editor', ({ page }) =>
  actions
    .mockAllAndLogin({ page })
    .openDataLinkModal()
    .withNameInput(async (input) => {
      await input.fill(DATA_LINK_NAME)
    }),
)
