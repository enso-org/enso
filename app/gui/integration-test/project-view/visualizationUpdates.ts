import { type Page } from 'integration-test/base'

/** Mock a data update for an attached visualization. */
export async function mockVisualizationDataUpdate(page: Page, preprocessor: string, data: unknown) {
  await page.evaluate(
    ({ preprocessor, data }) => (window as any)._mockVisualizationDataUpdate(preprocessor, data),
    { preprocessor, data },
  )
}
