/** @file A panel to switch between settings tabs. */
import * as React from 'react'

import KeyboardShortcutsIcon from 'enso-assets/keyboard_shortcuts.svg'
import LogIcon from 'enso-assets/log.svg'
import PeopleSettingsIcon from 'enso-assets/people_settings.svg'
import PeopleIcon from 'enso-assets/people.svg'
import SettingsIcon from 'enso-assets/settings.svg'

import * as textProvider from '#/providers/TextProvider'

import SettingsTab from '#/layouts/Settings/SettingsTab'

import * as aria from '#/components/aria'
import FocusArea from '#/components/styled/FocusArea'
import SidebarTabButton from '#/components/styled/SidebarTabButton'

// =================
// === Constants ===
// =================

const SECTIONS: SettingsSectionData[] = [
  {
    name: 'General',
    tabs: [
      {
        name: 'Account',
        settingsTab: SettingsTab.account,
        icon: SettingsIcon,
      },
      {
        name: 'Organization',
        settingsTab: SettingsTab.organization,
        icon: PeopleSettingsIcon,
      },
    ],
  },
  {
    name: 'Access',
    tabs: [
      {
        name: 'Members',
        settingsTab: SettingsTab.members,
        icon: PeopleIcon,
        organizationOnly: true,
      },
      {
        name: 'User Groups',
        settingsTab: SettingsTab.userGroups,
        icon: PeopleSettingsIcon,
        organizationOnly: true,
      },
    ],
  },
  {
    name: 'Look and feel',
    tabs: [
      {
        name: 'Keyboard shortcuts',
        settingsTab: SettingsTab.keyboardShortcuts,
        icon: KeyboardShortcutsIcon,
      },
    ],
  },
  {
    name: 'Security',
    tabs: [
      {
        name: 'Activity log',
        settingsTab: SettingsTab.activityLog,
        icon: LogIcon,
        organizationOnly: true,
      },
    ],
  },
]

// =============
// === Types ===
// =============

/** Metadata for rendering a settings tab label. */
interface SettingsTabLabelData {
  readonly name: string
  readonly settingsTab: SettingsTab
  readonly icon: string
  readonly organizationOnly?: true
}

/** Metadata for rendering a settings section. */
interface SettingsSectionData {
  readonly name: string
  readonly tabs: SettingsTabLabelData[]
}

// =======================
// === SettingsSidebar ===
// =======================

/** Props for a {@link SettingsSidebar} */
export interface SettingsSidebarProps {
  readonly isMenu?: true
  readonly isUserInOrganization: boolean
  readonly settingsTab: SettingsTab
  readonly setSettingsTab: React.Dispatch<React.SetStateAction<SettingsTab>>
  readonly onClickCapture?: () => void
}

/** A panel to switch between settings tabs. */
export default function SettingsSidebar(props: SettingsSidebarProps) {
  const { isMenu = false, isUserInOrganization, settingsTab, setSettingsTab } = props
  const { onClickCapture } = props
  const { getText } = textProvider.useText()

  return (
    <FocusArea direction="vertical">
      {innerProps => (
        <div
          aria-label={getText('settingsSidebarLabel')}
          className={`w-settings-sidebar shrink-0 flex-col gap-settings-sidebar overflow-y-auto ${
            !isMenu
              ? 'hidden sm:flex'
              : 'relative rounded-default p-modal text-xs text-primary before:absolute before:inset before:rounded-default before:bg-frame before:backdrop-blur-default sm:hidden'
          }`}
          onClickCapture={onClickCapture}
          {...innerProps}
        >
          {SECTIONS.map(section => (
            <div key={section.name} className="flex flex-col items-start">
              <aria.Header
                id={`${section.name}_header`}
                className="relative mb-sidebar-section-heading-b h-text px-sidebar-section-heading-x py-sidebar-section-heading-y text-sm font-bold leading-cozy"
              >
                {section.name}
              </aria.Header>
              {section.tabs.map(tab => (
                <SidebarTabButton
                  key={tab.settingsTab}
                  isDisabled={(tab.organizationOnly ?? false) && !isUserInOrganization}
                  id={tab.settingsTab}
                  icon={tab.icon}
                  label={tab.name}
                  active={tab.settingsTab === settingsTab}
                  onPress={() => {
                    setSettingsTab(tab.settingsTab)
                  }}
                />
              ))}
            </div>
          ))}
        </div>
      )}
    </FocusArea>
  )
}
