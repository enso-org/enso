/** @file A panel to switch between settings tabs. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import * as settingsData from '#/layouts/Settings/settingsData'
import type SettingsTabType from '#/layouts/Settings/SettingsTabType'

import * as aria from '#/components/aria'
import FocusArea from '#/components/styled/FocusArea'
import SidebarTabButton from '#/components/styled/SidebarTabButton'

// =======================
// === SettingsSidebar ===
// =======================

/** Props for a {@link SettingsSidebar} */
export interface SettingsSidebarProps {
  readonly settingsTab: SettingsTabType
  readonly setSettingsTab: React.Dispatch<React.SetStateAction<SettingsTabType>>
}

/** A panel to switch between settings tabs. */
export default function SettingsSidebar(props: SettingsSidebarProps) {
  const { settingsTab, setSettingsTab } = props
  const { getText } = textProvider.useText()

  return (
    <FocusArea direction="vertical">
      {innerProps => (
        <div
          aria-label={getText('settingsSidebarLabel')}
          className="flex w-settings-sidebar shrink-0 flex-col gap-settings-sidebar overflow-y-auto"
          {...innerProps}
        >
          {settingsData.SETTINGS_DATA.map(section => {
            const name = getText(section.nameId)
            return (
              <div key={name} className="flex flex-col items-start">
                <aria.Header
                  id={`${name}_header`}
                  className="mb-sidebar-section-heading-b h-text px-sidebar-section-heading-x py-sidebar-section-heading-y text-sm font-bold leading-cozy"
                >
                  {name}
                </aria.Header>
                {section.tabs.map(tab => (
                  <SidebarTabButton
                    key={tab.settingsTab}
                    id={tab.settingsTab}
                    icon={tab.icon}
                    label={getText(tab.nameId)}
                    active={tab.settingsTab === settingsTab}
                    onPress={() => {
                      setSettingsTab(tab.settingsTab)
                    }}
                  />
                ))}
              </div>
            )
          })}
        </div>
      )}
    </FocusArea>
  )
}
