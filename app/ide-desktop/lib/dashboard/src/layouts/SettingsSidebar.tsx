/** @file A panel to switch between settings tabs. */
import * as React from 'react'

import BellIcon from 'enso-assets/bell.svg'
import KeyboardShortcutsIcon from 'enso-assets/keyboard_shortcuts.svg'
import LogIcon from 'enso-assets/log.svg'
import PeopleSettingsIcon from 'enso-assets/people_settings.svg'
import PeopleIcon from 'enso-assets/people.svg'
import SettingsIcon from 'enso-assets/settings.svg'
import SlidersIcon from 'enso-assets/sliders.svg'

import SettingsTab from '#/layouts/Settings/SettingsTab'

import SvgMask from '#/components/SvgMask'

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
        visible: true,
      },
      {
        name: 'Organization',
        settingsTab: SettingsTab.organization,
        icon: PeopleSettingsIcon,
        visible: true,
      },
      {
        name: 'Features',
        settingsTab: SettingsTab.features,
        icon: SlidersIcon,
      },
      {
        name: 'Notifications',
        settingsTab: SettingsTab.notifications,
        icon: BellIcon,
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
        visible: true,
      },
      {
        name: 'Member Roles',
        settingsTab: SettingsTab.memberRoles,
        icon: PeopleSettingsIcon,
        visible: true,
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
        visible: true,
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
        visible: true,
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
  /** Temporary, until all tabs are implemented. */
  readonly visible?: true
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
  readonly settingsTab: SettingsTab
  readonly setSettingsTab: React.Dispatch<React.SetStateAction<SettingsTab>>
}

/** A panel to switch between settings tabs. */
export default function SettingsSidebar(props: SettingsSidebarProps) {
  const { settingsTab, setSettingsTab } = props
  return (
    <div className="flex w-settings-sidebar shrink-0 flex-col gap-settings-sidebar overflow-y-auto">
      {SECTIONS.flatMap(section => {
        const tabs = section.tabs.filter(tab => tab.visible)
        return tabs.length === 0
          ? []
          : [
              <div
                key={section.name}
                className="flex flex-col items-start gap-sidebar-section-heading"
              >
                <h2 className="h-text px-sidebar-section-heading-x py-sidebar-section-heading-y text-sm font-bold leading-cozy">
                  {section.name}
                </h2>
                <ul className="flex flex-col items-start">
                  {tabs.map(tab => (
                    <li key={tab.settingsTab}>
                      <button
                        disabled={tab.settingsTab === settingsTab}
                        className="button icon-with-text h-row px-button-x transition-colors selectable hover:bg-selected-frame disabled:bg-selected-frame disabled:active"
                        onClick={() => {
                          setSettingsTab(tab.settingsTab)
                        }}
                      >
                        <SvgMask
                          src={tab.icon}
                          className={
                            tab.settingsTab === settingsTab
                              ? 'text-icon-selected'
                              : 'text-icon-not-selected'
                          }
                        />
                        <span className="text">{tab.name}</span>
                      </button>
                    </li>
                  ))}
                </ul>
              </div>,
            ]
      })}
    </div>
  )
}
