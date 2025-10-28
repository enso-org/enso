/** @file Test that welcome project is loaded on fresh install. */
import { test } from 'integration-test/base'

test.describe(() => {
  test.use({
    setupApi: {
      addDefaultProject: false,
      cloud: (cloudApi) => {
        const samplesDir = cloudApi.addDirectory({ title: 'Samples' })
        cloudApi.addProject({
          parentId: samplesDir.id,
          title: 'Getting Started.project',
        })
      },
    },
  })
  test('Welcome project opens', async ({ drivePage }) => {
    await drivePage.expectProjectEditorOpened('Getting Started')
  })
})
