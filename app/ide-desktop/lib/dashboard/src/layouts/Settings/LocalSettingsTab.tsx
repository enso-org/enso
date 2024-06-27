/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import invariant from 'tiny-invariant'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import SettingsInput from '#/components/styled/settings/SettingsInput'
import SettingsSection from '#/components/styled/settings/SettingsSection'

import * as projectManager from '#/services/ProjectManager'

// ========================
// === LocalSettingsTab ===
// ========================

/** Settings tab for viewing and editing account information. */
export default function LocalSettingsTab() {
  const localBackend = backendProvider.useLocalBackend()
  const { getText } = textProvider.useText()
  const rootPathRef = React.useRef<HTMLInputElement | null>(null)

  invariant(localBackend, '`LocalSettingsTab` requires a `localBackend` to function.')

  const doUpdaterootPathMutation = reactQuery.useMutation({
    mutationKey: [localBackend.type, 'updateRootPath'],
    mutationFn: (value: string) => {
      localBackend.rootPath = projectManager.Path(value)
      return Promise.resolve()
    },
    meta: { invalidates: [[localBackend.type, 'listDirectory']], awaitInvalidates: true },
  })

  return (
    <SettingsSection title={getText('local')}>
      <aria.TextField
        key={localBackend.rootPath}
        defaultValue={localBackend.rootPath}
        className="flex h-row gap-settings-entry"
      >
        <aria.Label className="text my-auto w-organization-settings-label">
          {getText('rootDirectory')}
        </aria.Label>
        <SettingsInput
          key={localBackend.rootPath}
          ref={rootPathRef}
          type="text"
          onSubmit={doUpdaterootPathMutation.mutate}
        />
      </aria.TextField>
    </SettingsSection>
  )
}
