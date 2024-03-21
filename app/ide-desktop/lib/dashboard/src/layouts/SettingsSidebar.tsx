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

import * as aria from '#/components/aria'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'
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
const ALL_TABS = SECTIONS.flatMap(section =>
  section.tabs.filter(tab => tab.visible).map(tab => tab.settingsTab)
)

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
  const rootRef = React.useRef<HTMLDivElement>(null)
  const selectedChildIndexRef = React.useRef(0)

  React.useEffect(() => {
    selectedChildIndexRef.current = ALL_TABS.indexOf(settingsTab)
  }, [settingsTab])

  React.useEffect(() => {
    rootRef.current?.focus()
  }, [])

  return (
    <FocusArea direction="vertical">
      {(ref, innerProps) => (
        <div
          ref={ref}
          className="flex w-settings-sidebar shrink-0 flex-col gap-settings-sidebar overflow-y-auto"
          {...innerProps}
        >
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
                        <FocusRing within key={tab.settingsTab}>
                          <li className="rounded-full">
                            <aria.Button
                              className={`button icon-with-text h-row px-button-x transition-colors selectable hover:bg-selected-frame ${tab.settingsTab === settingsTab ? 'disabled bg-selected-frame active' : ''}`}
                              onPress={() => {
                                setSettingsTab(tab.settingsTab)
                              }}
                            >
                              <SvgMask src={tab.icon} />
                              <span className="text">{tab.name}</span>
                            </aria.Button>
                          </li>
                        </FocusRing>
                      ))}
                    </ul>
                  </div>,
                ]
          })}
        </div>
      )}
    </FocusArea>
  )
}
