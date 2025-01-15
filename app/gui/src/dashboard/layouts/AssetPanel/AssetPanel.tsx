/**
 * @file
 * The asset panel is a sidebar that can be expanded or collapsed.
 * It is used to view and interact with assets in the drive.
 */
import { memo, startTransition } from 'react'

import { AnimatePresence, motion } from 'framer-motion'

import type { BackendType } from 'enso-common/src/services/Backend'

import docsIcon from '#/assets/file_text.svg'
import sessionsIcon from '#/assets/group.svg'
import inspectIcon from '#/assets/inspect.svg'
import versionsIcon from '#/assets/versions.svg'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { AssetDocs } from '#/layouts/AssetDocs'
import AssetProjectSessions from '#/layouts/AssetProjectSessions'
import AssetProperties from '#/layouts/AssetProperties'
import AssetVersions from '#/layouts/AssetVersions/AssetVersions'
import { isLocalCategory, type Category } from '#/layouts/CategorySwitcher/Category'
import { useBackend } from '#/providers/BackendProvider'
import { useText } from '#/providers/TextProvider'
import { useStore } from '#/utilities/zustand'
import { assetPanelStore, useIsAssetPanelOpen, useSetIsAssetPanelOpen } from './AssetPanelState'
import { AssetPanelTabs } from './components/AssetPanelTabs'
import { AssetPanelToggle } from './components/AssetPanelToggle'
import { type AssetPanelTab } from './types'

const ASSET_SIDEBAR_COLLAPSED_WIDTH = 48
const ASSET_PANEL_WIDTH = 480
const ASSET_PANEL_TOTAL_WIDTH = ASSET_PANEL_WIDTH + ASSET_SIDEBAR_COLLAPSED_WIDTH

/** Props for an {@link AssetPanel}. */
export interface AssetPanelProps {
  readonly backendType: BackendType
  readonly category: Category
}

/**
 * A sidebar that can be expanded or collapsed.
 * It is used to view and interact with assets in the drive.
 */
export const AssetPanel = memo(function AssetPanel(props: AssetPanelProps) {
  const isHidden = useStore(assetPanelStore, (state) => state.isAssetPanelHidden, {
    unsafeEnableTransition: true,
  })
  const isOpen = useIsAssetPanelOpen()
  const panelWidth = isOpen ? ASSET_PANEL_TOTAL_WIDTH : ASSET_SIDEBAR_COLLAPSED_WIDTH
  const isVisible = !isHidden

  const compensationWidth = isVisible ? panelWidth : 0

  return (
    // We use a hex color here to avoid muliplying bg colors due to opacity.
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
            <AssetPanelTabsInternal panelWidth={panelWidth} {...props} />
          </motion.div>
        )}
      </AnimatePresence>
    </div>
  )
})

/** Tabs for an {@link AssetPanel}. */
const AssetPanelTabsInternal = memo(function AssetPanelTabsInternal(
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

  const isOpen = useIsAssetPanelOpen()
  const setIsOpen = useSetIsAssetPanelOpen()

  const expandTab = useEventCallback(() => {
    setIsOpen(true)
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
          if (key === selectedTab && isOpen) {
            setIsOpen(false)
          } else {
            // This is safe because we know the key is a valid AssetPanelTab.
            // eslint-disable-next-line no-restricted-syntax
            setSelectedTab(key as AssetPanelTab)
            setIsOpen(true)
          }
        })
      }}
    >
      <AnimatePresence initial={!isOpen} mode="sync">
        {isOpen && (
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
                  <AssetProjectSessions backend={backend} />
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
            icon={inspectIcon}
            label={isLocal ? getText('assetProperties.localBackend') : getText('properties')}
            isExpanded={isOpen}
            isDisabled={isLocal}
            onPress={expandTab}
          />
          <AssetPanelTabs.Tab
            id="versions"
            icon={versionsIcon}
            label={
              isLocal ? getText('assetVersions.localAssetsDoNotHaveVersions') : getText('versions')
            }
            isExpanded={isOpen}
            isDisabled={isLocal}
            onPress={expandTab}
          />
          <AssetPanelTabs.Tab
            id="sessions"
            icon={sessionsIcon}
            label={
              isLocal ? getText('assetProjectSessions.localBackend') : getText('projectSessions')
            }
            isExpanded={isOpen}
            isDisabled={isLocal}
            onPress={expandTab}
          />
          <AssetPanelTabs.Tab
            id="docs"
            icon={docsIcon}
            label={getText('docs')}
            isExpanded={isOpen}
            onPress={expandTab}
          />
        </AssetPanelTabs.TabList>
      </div>
    </AssetPanelTabs>
  )
})
