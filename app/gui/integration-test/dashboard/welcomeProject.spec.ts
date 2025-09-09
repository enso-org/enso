/** @file Test that welcome project is loaded on fresh install. */
import { test } from 'integration-test/base'

test('Welcome project opens', ({ cloudApi, drivePage }) => {
  const samplesDir = cloudApi.addDirectory({ title: 'Samples' })
  cloudApi.addProject({
    parentId: samplesDir.id,
    title: 'Getting Started.project',
  })

  drivePage.expectProjectEditorOpened('Getting Started')
})
