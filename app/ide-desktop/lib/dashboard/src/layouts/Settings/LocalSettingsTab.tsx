/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

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
  const rootDirectoryRef = React.useRef<HTMLInputElement | null>(null)

  invariant(localBackend, '`LocalSettingsTab` requires a `localBackend` to function.')

  const doUpdateRootDirectory = (value: string) => {
    localBackend.rootDirectory = projectManager.Path(value)
  }

  return (
    <SettingsSection title={getText('local')}>
      <aria.TextField
        key={localBackend.rootDirectory}
        defaultValue={localBackend.rootDirectory}
        className="flex h-row gap-settings-entry"
      >
        <aria.Label className="text my-auto w-organization-settings-label">
          {getText('rootDirectory')}
        </aria.Label>
        <SettingsInput
          key={localBackend.rootDirectory}
          ref={rootDirectoryRef}
          type="text"
          onSubmit={doUpdateRootDirectory}
        />
      </aria.TextField>
    </SettingsSection>
  )
}
