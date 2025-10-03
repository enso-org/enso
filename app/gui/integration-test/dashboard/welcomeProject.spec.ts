/** @file Test that welcome project is loaded on fresh install. */
import { test } from 'playwright/test'

import { mockAllAndLogin } from './actions'

test('Welcome project opens', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      const samplesDir = api.addDirectory({ title: 'Samples' })
      api.addProject({
        parentId: samplesDir.id,
        title: 'Getting Started.project',
      })
    },
    goToCloudFirst: false,
  }).expectProjectEditorOpened('Getting Started'))
