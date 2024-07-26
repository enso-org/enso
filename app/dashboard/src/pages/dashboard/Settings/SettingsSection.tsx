/** @file Rendering for a settings section. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import FocusArea from '#/components/styled/FocusArea'

import type * as settingsData from './settingsData'
import SettingsEntry from './SettingsEntry'

// =======================
// === SettingsSection ===
// =======================

/** Props for a {@link SettingsSection}. */
export interface SettingsSectionProps {
  readonly context: settingsData.SettingsContext
  readonly data: settingsData.SettingsSectionData
}

/** Rendering for a settings section. */
export default function SettingsSection(props: SettingsSectionProps) {
  const { context, data } = props
  const { nameId, focusArea = true, heading = true, entries } = data
  const { getText } = textProvider.useText()

  return (
    <FocusArea active={focusArea} direction="vertical">
      {(innerProps) => (
        <div className="flex w-full flex-col gap-settings-section-header" {...innerProps}>
          {!heading ? null : (
            <aria.Heading level={2} className="h-[2.375rem] py-0.5 text-xl font-bold">
              {getText(nameId)}
            </aria.Heading>
          )}
          <div className="flex flex-col">
            {entries.map((entry, i) => (
              <SettingsEntry key={i} context={context} data={entry} />
            ))}
          </div>
        </div>
      )}
    </FocusArea>
  )
}
