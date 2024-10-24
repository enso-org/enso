/**
 * @file
 * @description
 * The asset panel is a sidebar that can be expanded or collapsed.
 * It is used to view and interact with assets in the drive.
 */
import docsIcon from '#/assets/file_text.svg'
import sessionsIcon from '#/assets/group.svg'
import inspectIcon from '#/assets/inspect.svg'
import versionsIcon from '#/assets/versions.svg'

import { useBackend } from '#/providers/BackendProvider'
import {
  useAssetPanelProps,
  useIsAssetPanelExpanded,
  useIsAssetPanelHidden,
  useSetIsAssetPanelExpanded,
} from '#/providers/DriveProvider'
import { useLocalStorageState } from '#/providers/LocalStorageProvider'
import { useText } from '#/providers/TextProvider'
import LocalStorage from '#/utilities/LocalStorage'
import type { BackendType } from 'enso-common/src/services/Backend'
import type { Spring } from 'framer-motion'
import { AnimatePresence, motion } from 'framer-motion'
import { startTransition } from 'react'
import { z } from 'zod'
import { AssetDocs } from '../AssetDocs'
import AssetProjectSessions from '../AssetProjectSessions'
import AssetProperties from '../AssetProperties'
import AssetVersions from '../AssetVersions/AssetVersions'
import type { Category } from '../CategorySwitcher/Category'
import { AssetPanelTabs } from './components/AssetPanelTabs'
import { AssetPanelToggle } from './components/AssetPanelToggle'

const ASSET_SIDEBAR_COLLAPSED_WIDTH = 48
const ASSET_PANEL_WIDTH = 480
const ASSET_PANEL_TOTAL_WIDTH = ASSET_PANEL_WIDTH + ASSET_SIDEBAR_COLLAPSED_WIDTH

/** Determines the content of the {@link AssetPanel}. */
const ASSET_PANEL_TABS = ['settings', 'versions', 'sessions', 'schedules', 'docs'] as const

/** Determines the content of the {@link AssetPanel}. */
type AssetPanelTab = (typeof ASSET_PANEL_TABS)[number]

// ============================
// === Global configuration ===
// ============================

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly assetPanelTab: AssetPanelTab
    readonly assetPanelWidth: number
  }
}

const ASSET_PANEL_TAB_SCHEMA = z.enum(ASSET_PANEL_TABS)

LocalStorage.register({
  assetPanelTab: { schema: ASSET_PANEL_TAB_SCHEMA },
  assetPanelWidth: { schema: z.number().int() },
})

/**
 * Props for an {@link AssetPanel}.
 */
export interface AssetPanelProps {
  readonly backendType: BackendType
  readonly category: Category
}

const DEFAULT_TRANSITION_OPTIONS: Spring = {
  type: 'spring',
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  stiffness: 200,
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  damping: 30,
  mass: 1,
  velocity: 0,
}

/**
 * The asset panel is a sidebar that can be expanded or collapsed.
 * It is used to view and interact with assets in the drive.
 */
export function AssetPanel(props: AssetPanelProps) {
  const { category } = props

  const [selectedTab, setSelectedTab] = useLocalStorageState('assetPanelTab', ASSET_PANEL_TABS[0])

  const isReadonly = category.type === 'trash'

  const { getText } = useText()

  const isHidden = useIsAssetPanelHidden()
  const isExpanded = useIsAssetPanelExpanded()
  const setIsExpanded = useSetIsAssetPanelExpanded()

  const backend = useBackend(category)

  const { item } = useAssetPanelProps()

  const panelWidth = isExpanded ? ASSET_PANEL_TOTAL_WIDTH : ASSET_SIDEBAR_COLLAPSED_WIDTH
  const isVisible = !isHidden

  return (
    <AnimatePresence initial={isVisible} mode="sync">
      {isVisible && (
        <motion.div
          initial="initial"
          animate="animate"
          exit="exit"
          custom={panelWidth}
          variants={{
            initial: { opacity: 0, width: 0 },
            animate: (width: number) => ({ opacity: 1, width }),
            exit: { opacity: 0, width: 0 },
          }}
          transition={DEFAULT_TRANSITION_OPTIONS}
          className="relative flex flex-col shadow-softer clip-path-left-shadow"
          onClick={(event) => {
            // Prevent deselecting Assets Table rows.
            event.stopPropagation()
          }}
        >
          <AssetPanelTabs
            className="h-full"
            orientation="vertical"
            defaultSelectedKey={selectedTab}
            onSelectionChange={(key) => {
              startTransition(() => {
                console.log('onSelectionChange', { key, selectedTab, isExpanded, isHidden })
                if (key === selectedTab && isExpanded) {
                  setIsExpanded(false)
                } else {
                  setSelectedTab(ASSET_PANEL_TAB_SCHEMA.parse(key))
                  setIsExpanded(true)
                }
              })
            }}
          >
            {isExpanded && (
              <div
                className="min-h-full"
                // We use clipPath to prevent the sidebar from being visible under tabs while expanding.
                style={{ clipPath: `inset(0 ${ASSET_SIDEBAR_COLLAPSED_WIDTH}px 0 0)` }}
              >
                <motion.div
                  initial={{ x: ASSET_PANEL_WIDTH, filter: 'blur(16px)' }}
                  animate={{ x: 0, filter: 'blur(0px)' }}
                  exit={{ x: ASSET_PANEL_WIDTH, filter: 'blur(16px)' }}
                  transition={DEFAULT_TRANSITION_OPTIONS}
                  className="absolute left-0 top-0 h-full w-full bg-background"
                  style={{ width: ASSET_PANEL_WIDTH }}
                >
                  <AssetPanelTabs.TabPanel id="settings">
                    <AssetProperties
                      backend={backend}
                      item={item}
                      isReadonly={isReadonly}
                      category={category}
                    />
                  </AssetPanelTabs.TabPanel>

                  <AssetPanelTabs.TabPanel id="versions">
                    <AssetVersions backend={backend} item={item} />
                  </AssetPanelTabs.TabPanel>

                  <AssetPanelTabs.TabPanel id="sessions">
                    <AssetProjectSessions backend={backend} item={item} />
                  </AssetPanelTabs.TabPanel>

                  <AssetPanelTabs.TabPanel id="docs">
                    <AssetDocs backend={backend} item={item} />
                  </AssetPanelTabs.TabPanel>
                </motion.div>
              </div>
            )}

            <div
              className="absolute bottom-0 right-0 top-0 pt-2.5"
              style={{ width: ASSET_SIDEBAR_COLLAPSED_WIDTH }}
            >
              <AssetPanelToggle
                showWhen="expanded"
                className="flex aspect-square w-full items-center justify-center"
              />

              <AssetPanelTabs.TabList>
                <AssetPanelTabs.Tab
                  id="settings"
                  icon={inspectIcon}
                  label={getText('properties')}
                  isExpanded={isExpanded}
                  onPress={() => {
                    setIsExpanded(true)
                  }}
                />
                <AssetPanelTabs.Tab
                  id="versions"
                  icon={versionsIcon}
                  label={getText('versions')}
                  isExpanded={isExpanded}
                  onPress={() => {
                    setIsExpanded(true)
                  }}
                />
                <AssetPanelTabs.Tab
                  id="sessions"
                  icon={sessionsIcon}
                  label={getText('projectSessions')}
                  isExpanded={isExpanded}
                  onPress={() => {
                    setIsExpanded(true)
                  }}
                />
                <AssetPanelTabs.Tab
                  id="docs"
                  icon={docsIcon}
                  label={getText('docs')}
                  isExpanded={isExpanded}
                  onPress={() => {
                    setIsExpanded(true)
                  }}
                />
              </AssetPanelTabs.TabList>
            </div>
          </AssetPanelTabs>
        </motion.div>
      )}
    </AnimatePresence>
  )
}
