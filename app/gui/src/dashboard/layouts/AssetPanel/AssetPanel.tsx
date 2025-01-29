/**
 * @file
 * A sidebar that can be expanded or collapsed.
 * It is used to view and interact with assets in the drive.
 */
import { AnimatePresence, motion } from 'framer-motion'
import { memo, startTransition } from 'react'

import type { BackendType } from 'enso-common/src/services/Backend'

import RepeatIcon from '#/assets/arrows_repeat.svg'
import CalendarIcon from '#/assets/calendar_repeat_outline.svg'
import DocsIcon from '#/assets/file_text.svg'
import SessionsIcon from '#/assets/group.svg'
import InspectIcon from '#/assets/inspect.svg'
import VersionsIcon from '#/assets/versions.svg'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { AssetDocs } from '#/layouts/AssetDocs'
import { isLocalCategory, type Category } from '#/layouts/CategorySwitcher/Category'
import { useBackend } from '#/providers/BackendProvider'
import { useText } from '#/providers/TextProvider'
import { useStore } from '#/utilities/zustand'
import { useFeatureFlag } from '../../providers/FeatureFlagsProvider'
import {
  assetPanelStore,
  useIsAssetPanelExpanded,
  useSetIsAssetPanelExpanded,
} from './AssetPanelState'
import { AssetPanelTabs } from './components/AssetPanelTabs'
import { AssetPanelToggle } from './components/AssetPanelToggle'
import { AssetProperties } from './components/AssetProperties'
import { AssetVersions } from './components/AssetVersions'
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
export const AssetPanel = memo(function AssetPanel(props: AssetPanelProps) {
  const isHidden = useStore(assetPanelStore, (state) => state.isAssetPanelHidden, {
    unsafeEnableTransition: true,
  })
  const isExpanded = useIsAssetPanelExpanded()

  const panelWidth = isExpanded ? ASSET_PANEL_TOTAL_WIDTH : ASSET_SIDEBAR_COLLAPSED_WIDTH
  const isVisible = !isHidden

  const compensationWidth = isVisible ? panelWidth : 0

  return (
    // We use hex color here to avoid muliplying bg colors due to opacity.
    <div className="relative flex h-full flex-col">
      <div style={{ width: compensationWidth, height: 0 }} />

      {isVisible && (
        <div
          className="absolute bottom-0 right-0 top-0 bg-dashboard shadow-softer clip-path-left-shadow"
          style={{ width: ASSET_SIDEBAR_COLLAPSED_WIDTH }}
        />
      )}

      <AnimatePresence initial={!isVisible}>
        {isVisible && (
          <motion.div
            style={{ width: panelWidth }}
            data-testid="asset-panel"
            initial={{ opacity: 0, x: ASSET_SIDEBAR_COLLAPSED_WIDTH }}
            animate={{ opacity: 1, x: 0 }}
            exit={{ opacity: 0, x: ASSET_SIDEBAR_COLLAPSED_WIDTH }}
            className="absolute bottom-0 right-0 top-0 flex flex-col"
            onClick={(event: Event) => {
              // Prevent deselecting Assets Table rows.
              event.stopPropagation()
            }}
          >
            <InternalAssetPanelTabs panelWidth={panelWidth} {...props} />
          </motion.div>
        )}
      </AnimatePresence>
    </div>
  )
})

/** The internal implementation of the Asset Panel Tabs. */
const InternalAssetPanelTabs = memo(function InternalAssetPanelTabs(
  props: AssetPanelProps & { panelWidth: number },
) {
  const { category, panelWidth } = props

  const itemId = useStore(assetPanelStore, (state) => state.assetPanelProps.item?.id, {
    unsafeEnableTransition: true,
  })

  const selectedTab = useStore(assetPanelStore, (state) => state.selectedTab, {
    unsafeEnableTransition: true,
  })
  const setSelectedTab = useStore(assetPanelStore, (state) => state.setSelectedTab, {
    unsafeEnableTransition: true,
  })
  const isHidden = useStore(assetPanelStore, (state) => state.isAssetPanelHidden, {
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

  const getTranslation = useEventCallback(() => ASSET_SIDEBAR_COLLAPSED_WIDTH)

  return (
    <AssetPanelTabs
      className="h-full"
      style={{ width: panelWidth }}
      orientation="vertical"
      selectedKey={selectedTab}
      defaultSelectedKey={selectedTab}
      onSelectionChange={(key) => {
        if (isHidden) {
          return
        }

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
      <AnimatePresence initial={!isExpanded} mode="sync">
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
              </ErrorBoundary>
            </div>
          </motion.div>
        )}
      </AnimatePresence>

      <div
        className="absolute bottom-0 right-0 top-0 bg-dashboard pt-2.5"
        style={{ width: ASSET_SIDEBAR_COLLAPSED_WIDTH }}
      >
        <AssetPanelToggle
          showWhen="expanded"
          className="flex aspect-square w-full items-center justify-center"
          getTranslation={getTranslation}
        />

        <AssetPanelTabs.TabList>
          <AssetPanelTabs.Tab
            id="settings"
            icon={InspectIcon}
            label={isLocal ? getText('assetProperties.localBackend') : getText('properties')}
            isExpanded={isExpanded}
            onPress={expandTab}
            isDisabled={isLocal}
          />
          <AssetPanelTabs.Tab
            id="versions"
            icon={VersionsIcon}
            label={
              isLocal ? getText('assetVersions.localAssetsDoNotHaveVersions') : getText('versions')
            }
            isExpanded={isExpanded}
            onPress={expandTab}
            isDisabled={isLocal}
          />
          <AssetPanelTabs.Tab
            id="sessions"
            icon={SessionsIcon}
            label={
              isLocal ? getText('assetProjectSessions.localBackend') : getText('projectSessions')
            }
            isExpanded={isExpanded}
            onPress={expandTab}
            isDisabled={isLocal}
          />
          <AssetPanelTabs.Tab
            id="executions"
            icon={RepeatIcon}
            label={isLocal ? getText('assetProjectExecutions.localBackend') : getText('executions')}
            isExpanded={isExpanded}
            onPress={expandTab}
            isDisabled={isLocal}
            isHidden={!enableAsyncExecution}
          />
          <AssetPanelTabs.Tab
            id="executionsCalendar"
            icon={CalendarIcon}
            label={
              isLocal ?
                getText('assetProjectExecutionsCalendar.localBackend')
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
        </AssetPanelTabs.TabList>
      </div>
    </AssetPanelTabs>
  )
})
