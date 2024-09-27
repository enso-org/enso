/** @file A panel to switch between settings tabs. */
import type { Dispatch, SetStateAction } from 'react'

import { Header } from '#/components/aria'
import { ButtonGroup } from '#/components/AriaComponents'
import FocusArea from '#/components/styled/FocusArea'
import SidebarTabButton from '#/components/styled/SidebarTabButton'
import { useText } from '#/providers/TextProvider'
import { twMerge } from '#/utilities/tailwindMerge'
import { SETTINGS_DATA, type SettingsContext } from './data'
import type SettingsTabType from './TabType'

// =======================
// === SettingsSidebar ===
// =======================

/** Props for a {@link SettingsSidebar} */
export interface SettingsSidebarProps {
  readonly context: SettingsContext
  readonly tabsToShow: readonly SettingsTabType[]
  readonly isMenu?: true
  readonly tab: SettingsTabType
  readonly setTab: Dispatch<SetStateAction<SettingsTabType>>
  readonly onClickCapture?: () => void
}

/** A panel to switch between settings tabs. */
export default function SettingsSidebar(props: SettingsSidebarProps) {
  const { context, tabsToShow, isMenu = false, tab, setTab } = props
  const { onClickCapture } = props
  const { getText } = useText()

  return (
    <FocusArea direction="vertical">
      {(innerProps) => (
        <div
          aria-label={getText('settingsSidebarLabel')}
          className={twMerge(
            'w-settings-sidebar shrink-0 flex-col gap-settings-sidebar overflow-y-auto',
            !isMenu ? 'hidden sm:flex' : (
              'relative rounded-default p-modal text-xs text-primary before:absolute before:inset before:rounded-default before:bg-frame before:backdrop-blur-default sm:hidden'
            ),
          )}
          onClickCapture={onClickCapture}
          {...innerProps}
        >
          {SETTINGS_DATA.map((section) => {
            const name = getText(section.nameId)
            const visibleTabData = section.tabs.filter(
              (tabData) =>
                tabsToShow.includes(tabData.settingsTab) &&
                (!tabData.visible || tabData.visible(context)),
            )
            return visibleTabData.length === 0 ?
                null
              : <div key={name} className="flex flex-col items-start">
                  <Header
                    id={`${name}_header`}
                    className="z-1 mb-sidebar-section-heading-b h-text px-sidebar-section-heading-x py-sidebar-section-heading-y text-[13.5px] font-bold leading-cozy"
                  >
                    {name}
                  </Header>

                  <ButtonGroup gap="xxsmall" direction="column" align="start">
                    {visibleTabData.map((tabData) => (
                      <SidebarTabButton
                        key={tabData.settingsTab}
                        id={tabData.settingsTab}
                        icon={tabData.icon}
                        label={getText(tabData.nameId)}
                        isActive={tabData.settingsTab === tab}
                        onPress={() =>
                          tabData.onPress ?
                            tabData.onPress(context)
                            // even though this function returns void, we don't want to
                            // complicate things by returning only in case of custom onPress
                            // eslint-disable-next-line @typescript-eslint/no-confusing-void-expression
                          : setTab(tabData.settingsTab)
                        }
                      />
                    ))}
                  </ButtonGroup>
                </div>
          })}
        </div>
      )}
    </FocusArea>
  )
}
