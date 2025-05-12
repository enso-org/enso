/**
 * @file
 * A sidebar that can be expanded or collapsed.
 * It is used to view and interact with assets in the drive.
 */
import { AnimatePresence, motion } from 'framer-motion'
import { startTransition } from 'react'

import type { BackendType } from 'enso-common/src/services/Backend'

import RepeatIcon from '#/assets/arrows_repeat.svg'
import CalendarIcon from '#/assets/calendar_repeat_outline.svg'
import DocsIcon from '#/assets/file_text.svg'
import SessionsIcon from '#/assets/group.svg'
import InspectIcon from '#/assets/inspect.svg'
import VersionsIcon from '#/assets/versions.svg'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { isLocalCategory, type Category } from '#/layouts/CategorySwitcher/Category'
import { useBackend } from '#/providers/BackendProvider'
import { useFeatureFlag } from '#/providers/FeatureFlagsProvider'
import { useText } from '#/providers/TextProvider'
import { useStore } from '#/utilities/zustand'
import {
  assetPanelStore,
  useIsAssetPanelExpanded,
  useSetIsAssetPanelExpanded,
} from './AssetPanelState'
import { AssetDocs } from './components/AssetDocs'
import { AssetPanelTabs } from './components/AssetPanelTabs'
import { AssetProperties } from './components/AssetProperties'
import { AssetVersions } from './components/AssetVersions'
import { ComponentHelp } from './components/ComponentHelp'
import { ProjectExecutions } from './components/ProjectExecutions'
import { ProjectExecutionsCalendar } from './components/ProjectExecutionsCalendar'
import { ProjectSessions } from './components/ProjectSessions'
import type { AssetPanelTab } from './types'

const ASSET_SIDEBAR_COLLAPSED_WIDTH = 48
const ASSET_PANEL_WIDTH = 480
const ASSET_PANEL_TOTAL_WIDTH = ASSET_PANEL_WIDTH + ASSET_SIDEBAR_COLLAPSED_WIDTH

/** Props for an {@link AssetPanel}. */
export interface AssetPanelProps {
  readonly backendType: BackendType
  readonly category: Category
}

/**
 * The asset panel is a sidebar that can be expanded or collapsed.
 * It is used to view and interact with assets in the drive.
 */
export function AssetPanel(props: AssetPanelProps) {
  const isExpanded = useIsAssetPanelExpanded()

  const panelWidth = isExpanded ? ASSET_PANEL_TOTAL_WIDTH : ASSET_SIDEBAR_COLLAPSED_WIDTH

  const compensationWidth = panelWidth

  return (
    // We use hex color here to avoid muliplying bg colors due to opacity.
    <div className="relative flex h-full flex-col">
      <div style={{ width: compensationWidth, height: 0 }} />

      <div
        className="absolute bottom-0 right-0 top-0 bg-dashboard"
        style={{ width: ASSET_SIDEBAR_COLLAPSED_WIDTH }}
      />

      <div
        style={{ width: panelWidth }}
        data-testid="asset-panel"
        className="absolute bottom-0 right-0 top-0 flex flex-col"
      >
        <InternalAssetPanelTabs panelWidth={panelWidth} {...props} />
      </div>
    </div>
  )
}

/** The internal implementation of the Asset Panel Tabs. */
const InternalAssetPanelTabs = function InternalAssetPanelTabs(
  props: AssetPanelProps & { panelWidth: number },
) {
  const { category, panelWidth } = props

  const itemId = useStore(
    assetPanelStore,
    (state) => state.assetPanelProps.item?.id ?? state.assetPanelProps.defaultItem?.id,
    { unsafeEnableTransition: true },
  )

  const selectedTab = useStore(assetPanelStore, (state) => state.selectedTab, {
    unsafeEnableTransition: true,
  })
  const setSelectedTab = useStore(assetPanelStore, (state) => state.setSelectedTab, {
    unsafeEnableTransition: true,
  })

  const isReadonly = category.type === 'trash'
  const isLocal = isLocalCategory(category)

  const { getText } = useText()

  const isExpanded = useIsAssetPanelExpanded()
  const setIsExpanded = useSetIsAssetPanelExpanded()

  const enableAsyncExecution = useFeatureFlag('enableAsyncExecution')

  const expandTab = useEventCallback(() => {
    setIsExpanded(true)
  })

  const backend = useBackend(category)

  return (
    <AssetPanelTabs
      className="h-full"
      style={{ width: panelWidth }}
      orientation="vertical"
      selectedKey={selectedTab}
      defaultSelectedKey={selectedTab}
      onSelectionChange={(key) => {
        startTransition(() => {
          if (key === selectedTab && isExpanded) {
            setIsExpanded(false)
          } else {
            // This is safe because we know the key is a valid AssetPanelTab.
            // eslint-disable-next-line no-restricted-syntax
            setSelectedTab(key as AssetPanelTab)
            setIsExpanded(true)
          }
        })
      }}
    >
      <AnimatePresence initial={true} mode="sync">
        {isExpanded && (
          <motion.div
            initial="initial"
            animate="animate"
            exit="exit"
            variants={{
              initial: { filter: 'blur(8px)', x: ASSET_PANEL_WIDTH },
              animate: { filter: 'blur(0px)', x: 0 },
              exit: { filter: 'blur(8px)', x: ASSET_PANEL_WIDTH },
            }}
            className="absolute bottom-0 top-0 h-full"
            style={{
              // to avoid blurry edges
              clipPath: `inset(0 0 0 0)`,
              width: ASSET_PANEL_WIDTH,
              right: ASSET_SIDEBAR_COLLAPSED_WIDTH,
            }}
          >
            {/* We use hex color here to avoid muliplying bg colors due to opacity. */}
            <div className="flex h-full flex-col bg-background-hex">
              <ErrorBoundary resetKeys={[itemId]}>
                <AssetPanelTabs.TabPanel id="settings">
                  <AssetProperties backend={backend} isReadonly={isReadonly} category={category} />
                </AssetPanelTabs.TabPanel>

                <AssetPanelTabs.TabPanel id="versions">
                  <AssetVersions backend={backend} />
                </AssetPanelTabs.TabPanel>

                <AssetPanelTabs.TabPanel id="sessions">
                  <ProjectSessions backend={backend} />
                </AssetPanelTabs.TabPanel>

                <AssetPanelTabs.TabPanel id="executions">
                  <ProjectExecutions backend={backend} />
                </AssetPanelTabs.TabPanel>

                <AssetPanelTabs.TabPanel id="executionsCalendar">
                  <ProjectExecutionsCalendar backend={backend} />
                </AssetPanelTabs.TabPanel>

                <AssetPanelTabs.TabPanel id="docs">
                  <AssetDocs backend={backend} />
                </AssetPanelTabs.TabPanel>

                <AssetPanelTabs.TabPanel id="componentHelp">
                  <ComponentHelp backend={backend} />
                </AssetPanelTabs.TabPanel>
              </ErrorBoundary>
            </div>
          </motion.div>
        )}
      </AnimatePresence>

      <div
        className="bg-dasboard absolute bottom-0 right-0 top-0 flex flex-col overflow-clip pt-3"
        style={{ width: ASSET_SIDEBAR_COLLAPSED_WIDTH }}
      >
        <AssetPanelTabs.TabList>
          <AssetPanelTabs.Tab
            id="settings"
            icon={InspectIcon}
            label={isLocal ? getText('assetProperties.cloudOnly') : getText('properties')}
            isExpanded={isExpanded}
            onPress={expandTab}
            isDisabled={isLocal}
          />
          <AssetPanelTabs.Tab
            id="versions"
            icon={VersionsIcon}
            label={isLocal ? getText('assetVersions.cloudOnly') : getText('versions')}
            isExpanded={isExpanded}
            onPress={expandTab}
            isDisabled={isLocal}
          />
          <AssetPanelTabs.Tab
            id="sessions"
            icon={SessionsIcon}
            label={isLocal ? getText('assetProjectSessions.cloudOnly') : getText('projectSessions')}
            isExpanded={isExpanded}
            onPress={expandTab}
            isDisabled={isLocal}
          />
          <AssetPanelTabs.Tab
            id="executions"
            icon={RepeatIcon}
            label={isLocal ? getText('assetProjectExecutions.cloudOnly') : getText('executions')}
            isExpanded={isExpanded}
            onPress={expandTab}
            isDisabled={isLocal}
            isHidden={true}
          />
          <AssetPanelTabs.Tab
            id="executionsCalendar"
            icon={CalendarIcon}
            label={
              isLocal ?
                getText('assetProjectExecutionsCalendar.cloudOnly')
              : getText('executionsCalendar')
            }
            isExpanded={isExpanded}
            onPress={expandTab}
            isDisabled={isLocal}
            isHidden={!enableAsyncExecution}
          />
          <AssetPanelTabs.Tab
            id="docs"
            icon={DocsIcon}
            label={getText('docs')}
            isExpanded={isExpanded}
            onPress={expandTab}
          />
          <AssetPanelTabs.Tab
            id="componentHelp"
            icon="help"
            label={getText('componentHelp')}
            isExpanded={isExpanded}
            onPress={expandTab}
          />
        </AssetPanelTabs.TabList>
      </div>
    </AssetPanelTabs>
  )
}
