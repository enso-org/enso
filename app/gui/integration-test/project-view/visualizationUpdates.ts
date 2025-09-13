import { type Page } from 'playwright/test'

/** Mock a data update for an attached visualization. */
export async function mockVisualizationDataUpdate(page: Page, preprocessor: string, data: unknown) {
  await page.evaluate(
    ({ preprocessor, data }) => (window as any)._mockVisualizationDataUpdate(preprocessor, data),
    { preprocessor, data },
  )
}

/** Clear standard widget configurations. Use `updateMockWidgetConfiguration` to set a specific configuration needed for test. */
export async function resetMockWidgetConfigurations(page: Page) {
  await page.evaluate(() => (window as any)._resetMockWidgetConfigurations())
}
