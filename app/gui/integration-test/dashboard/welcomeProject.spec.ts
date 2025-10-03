/** @file Test that welcome project is loaded on fresh install. */
import { test } from 'integration-test/base'

test('Welcome project opens', async ({ cloudApi, drivePage }) => {
  const samplesDir = cloudApi.addDirectory({ title: 'Samples' })
  cloudApi.addProject({
    parentId: samplesDir.id,
    title: 'Getting Started.project',
  })

  await drivePage.expectProjectEditorOpened('Getting Started')
})
