/** @file A panel to switch between settings tabs. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import * as settingsData from '#/layouts/Settings/settingsData'
import type SettingsTabType from '#/layouts/Settings/SettingsTabType'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import FocusArea from '#/components/styled/FocusArea'
import SidebarTabButton from '#/components/styled/SidebarTabButton'

import * as tailwindMerge from '#/utilities/tailwindMerge'

// =======================
// === SettingsSidebar ===
// =======================

/** Props for a {@link SettingsSidebar} */
export interface SettingsSidebarProps {
  readonly context: settingsData.SettingsContext
  readonly tabsToShow: readonly SettingsTabType[]
  readonly isMenu?: true
  readonly tab: SettingsTabType
  readonly setTab: React.Dispatch<React.SetStateAction<SettingsTabType>>
  readonly onClickCapture?: () => void
}

/** A panel to switch between settings tabs. */
export default function SettingsSidebar(props: SettingsSidebarProps) {
  const { context, tabsToShow, isMenu = false, tab, setTab } = props
  const { onClickCapture } = props
  const { getText } = textProvider.useText()

  return (
    <FocusArea direction="vertical">
      {innerProps => (
        <div
          aria-label={getText('settingsSidebarLabel')}
          className={tailwindMerge.twMerge(
            'w-settings-sidebar shrink-0 flex-col gap-settings-sidebar overflow-y-auto',
            !isMenu
              ? 'hidden sm:flex'
              : 'relative rounded-default p-modal text-xs text-primary before:absolute before:inset before:rounded-default before:bg-frame before:backdrop-blur-default sm:hidden'
          )}
          onClickCapture={onClickCapture}
          {...innerProps}
        >
          {settingsData.SETTINGS_DATA.map(section => {
            const name = getText(section.nameId)
            const visibleTabData = section.tabs.filter(
              tabData =>
                tabsToShow.includes(tabData.settingsTab) &&
                (!tabData.visible || tabData.visible(context))
            )
            return visibleTabData.length === 0 ? null : (
              <div key={name} className="flex flex-col items-start">
                <aria.Header
                  id={`${name}_header`}
                  className="mb-sidebar-section-heading-b h-text px-sidebar-section-heading-x py-sidebar-section-heading-y text-[13.5px] font-bold leading-cozy"
                >
                  {name}
                </aria.Header>

                <ariaComponents.ButtonGroup gap="xxsmall" direction="column" align="start">
                  {visibleTabData.map(tabData => (
                    <SidebarTabButton
                      key={tabData.settingsTab}
                      id={tabData.settingsTab}
                      icon={tabData.icon}
                      label={getText(tabData.nameId)}
                      active={tabData.settingsTab === tab}
                      onPress={() =>
                        tabData.onPress
                          ? tabData.onPress(context)
                          : // even though this function returns void, we don't want to
                            // complicate things by returning only in case of custom onPress
                            // eslint-disable-next-line @typescript-eslint/no-confusing-void-expression
                            setTab(tabData.settingsTab)
                      }
                    />
                  ))}
                </ariaComponents.ButtonGroup>
              </div>
            )
          })}
        </div>
      )}
    </FocusArea>
  )
}
