/** @file Actions for the third step of the "setup" page. */
import { TEXT } from '.'
import BaseActions from './BaseActions'
import SetupInvitePageActions from './SetupInvitePageActions'

/** Actions for the third step of the "setup" page. */
export default class SetupOrganizationPageActions<Context> extends BaseActions<Context> {
  /** Set the organization name for this organization. */
  setOrganizationName(organizationName: string) {
    return this.step(`Set organization name to '${organizationName}'`, async (page) => {
      await page
        .getByLabel(TEXT.organizationNameSettingsInput)
        .and(page.getByRole('textbox'))
        .fill(organizationName)
      await page.getByText(TEXT.next).click()
    }).into(SetupInvitePageActions<Context>)
  }
}
