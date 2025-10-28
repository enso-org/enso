/** @file Test the user settings tab. */
import { test } from 'integration-test/base'

const DATA_LINK_NAME = 'a data link'

test('data link editor', async ({ drivePage }) => {
  await drivePage.goToCategory
    .cloud()
    .openDataLinkModal()
    .withNameInput((input) => input.fill(DATA_LINK_NAME))
})
