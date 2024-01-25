/** @file A panel to switch between settings tabs. */
import * as React from 'react'

import BellIcon from 'enso-assets/bell.svg'
import PeopleIcon from 'enso-assets/people.svg'
import SettingsIcon from 'enso-assets/settings.svg'
import SlidersIcon from 'enso-assets/sliders.svg'

import SettingsTab from '#/layouts/dashboard/settingsTab/SettingsTab'

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
    ],
  },
]

// =============
// === Types ===
// =============

/** Metadata for rendering a settings tab label. */
interface SettingsTabLabelData {
  name: string
  settingsTab: SettingsTab
  icon: string
  /** Temporary, until all tabs are implemented. */
  visible?: true
}

/** Metadata for rendering a settings section. */
interface SettingsSectionData {
  name: string
  tabs: SettingsTabLabelData[]
}

// =======================
// === SettingsSidebar ===
// =======================

/** Props for a {@link SettingsSidebar} */
export interface SettingsSidebarProps {
  settingsTab: SettingsTab
  setSettingsTab: React.Dispatch<React.SetStateAction<SettingsTab>>
}

/** A panel to switch between settings tabs. */
export default function SettingsSidebar(props: SettingsSidebarProps) {
  const { settingsTab, setSettingsTab } = props
  return (
    <div className="flex flex-col gap-4 w-51.5 overflow-y-auto">
      {SECTIONS.flatMap(section => {
        const tabs = section.tabs.filter(tab => tab.visible)
        return tabs.length === 0
          ? []
          : [
              <div key={section.name} className="flex flex-col items-start">
                <h2 className="text-sm font-bold h-7.5 leading-5 pl-2 pt-0.5 pb-2">
                  {section.name}
                </h2>
                {tabs.map(tab => (
                  <div
                    key={tab.settingsTab}
                    className={`flex items-center gap-2 h-8 px-2 rounded-full hover:text-primary hover:bg-frame-selected transition-colors ${
                      tab.settingsTab === settingsTab
                        ? 'text-primary bg-frame-selected'
                        : 'cursor-pointer text-not-selected'
                    }`}
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
                    <span className="h-6 leading-5 py-px">{tab.name}</span>
                  </div>
                ))}
              </div>,
            ]
      })}
    </div>
  )
}
