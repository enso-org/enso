/** @file A panel to switch between settings tabs. */
import { Header } from '#/components/aria'
import { Button } from '#/components/Button'
import SidebarTabButton from '#/components/styled/SidebarTabButton'
import { Text } from '#/components/Text'
import { useText } from '$/providers/react'
import { memo, type Dispatch, type SetStateAction } from 'react'
import { SETTINGS_DATA, type SettingsContext } from './data'
import type SettingsTabType from './TabType'

/** Props for a {@link SettingsSidebar} */
export interface SettingsSidebarProps {
  readonly context: SettingsContext
  readonly tabsToShow: readonly SettingsTabType[]
  readonly tab: SettingsTabType
  readonly setTab: Dispatch<SetStateAction<SettingsTabType>>
  readonly onClickCapture?: () => void
}

/** A panel to switch between settings tabs. */
function SettingsSidebar(props: SettingsSidebarProps) {
  const { context, tabsToShow, tab, setTab } = props
  const { onClickCapture } = props
  const { getText } = useText()

  return (
    <div
      aria-label={getText('settingsSidebarLabel')}
      className="w-settings-sidebar shrink-0 flex-col gap-4 overflow-y-auto"
      onClickCapture={onClickCapture}
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
                <Text.Heading variant="subtitle">{name}</Text.Heading>
              </Header>

              <Button.Group gap="xxsmall" direction="column" align="start">
                {visibleTabData.map((tabData) => (
                  <SidebarTabButton
                    key={tabData.settingsTab}
                    id={tabData.settingsTab}
                    icon={tabData.icon}
                    label={getText(tabData.nameId)}
                    isActive={tabData.settingsTab === tab}
                    onPress={() => {
                      if (tab !== tabData.settingsTab) {
                        setTab(tabData.settingsTab)
                      }
                    }}
                  />
                ))}
              </Button.Group>
            </div>
      })}
    </div>
  )
}

export default memo(SettingsSidebar)
