/** @file A panel to switch between settings tabs. */
import * as React from 'react'

import BellIcon from 'enso-assets/bell.svg'
import KeyboardShortcutsIcon from 'enso-assets/keyboard_shortcuts.svg'
import PeopleSettingsIcon from 'enso-assets/people_settings.svg'
import PeopleIcon from 'enso-assets/people.svg'
import SettingsIcon from 'enso-assets/settings.svg'
import SlidersIcon from 'enso-assets/sliders.svg'

import * as keyboardNavigationHooks from '#/hooks/keyboardNavigationHooks'

import * as navigator2DProvider from '#/providers/Navigator2DProvider'

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
  const navigator2D = navigator2DProvider.useNavigator2D()

  const [keyboardSelectedIndex, setKeyboardSelectedIndex] =
    keyboardNavigationHooks.useKeyboardChildNavigation(rootRef, {
      defaultIndex: selectedChildIndexRef.current,
      length: ALL_TABS.length,
    })
  const keyboardSelectedTab = keyboardSelectedIndex == null ? null : ALL_TABS[keyboardSelectedIndex]

  React.useEffect(() => {
    const root = rootRef.current
    if (root == null) {
      return
    } else {
      return navigator2D.register(root, {
        focusPrimaryChild: () => {
          setKeyboardSelectedIndex(selectedChildIndexRef.current)
        },
        focusWhenPressed: {
          down: setKeyboardSelectedIndex.bind(null, 0),
          up: setKeyboardSelectedIndex.bind(null, ALL_TABS.length - 1),
        },
      })
    }
  }, [navigator2D, setKeyboardSelectedIndex])

  React.useEffect(() => {
    selectedChildIndexRef.current = ALL_TABS.indexOf(settingsTab)
  }, [settingsTab])

  React.useEffect(() => {
    rootRef.current?.focus()
  }, [])

  return (
    <div
      tabIndex={-1}
      ref={rootRef}
      className="flex w-settings-sidebar shrink-0 flex-col gap-settings-sidebar overflow-y-auto"
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
                    <li key={tab.settingsTab}>
                      <button
                        ref={element => {
                          if (tab.settingsTab === keyboardSelectedTab) {
                            element?.focus()
                          }
                        }}
                        className={`button icon-with-text h-row px-button-x transition-colors selectable hover:bg-selected-frame ${tab.settingsTab === settingsTab ? 'disabled bg-selected-frame active' : ''} ${tab.settingsTab === keyboardSelectedTab ? 'focus-ring' : ''}`}
                        onClick={() => {
                          setSettingsTab(tab.settingsTab)
                        }}
                      >
                        <SvgMask src={tab.icon} />
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
