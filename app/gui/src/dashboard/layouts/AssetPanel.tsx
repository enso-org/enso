/** @file A panel containing the description and settings for an asset. */
import { useEffect, useRef, useState } from 'react'

import * as z from 'zod'

import { TabPanel, Tabs } from '#/components/aria'
import ProjectSessions from '#/layouts/AssetProjectSessions'
import AssetProperties, { type AssetPropertiesSpotlight } from '#/layouts/AssetProperties'
import AssetVersions from '#/layouts/AssetVersions/AssetVersions'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import TabBar, { Tab } from '#/layouts/TabBar'
import { useAssetPanelProps, useIsAssetPanelVisible } from '#/providers/DriveProvider'
import { useLocalStorage } from '#/providers/LocalStorageProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { AssetType, BackendType } from '#/services/Backend'
import type { AnyAssetTreeNode } from '#/utilities/AssetTreeNode'
import LocalStorage from '#/utilities/LocalStorage'
import { twMerge } from '#/utilities/tailwindMerge'

// =====================
// === AssetPanelTab ===
// =====================

const ASSET_PANEL_TABS = ['settings', 'versions', 'sessions', 'schedules'] as const
const TABS_SCHEMA = z.enum(ASSET_PANEL_TABS)

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

LocalStorage.register({
  assetPanelTab: { schema: z.enum(ASSET_PANEL_TABS) },
  assetPanelWidth: { schema: z.number().int() },
})

// ==================
// === AssetPanel ===
// ==================

/** Props supplied by the row. */
export interface AssetPanelContextProps {
  readonly backend: Backend | null
  readonly item: AnyAssetTreeNode | null
  readonly spotlightOn?: AssetPropertiesSpotlight
}

/** Props for an {@link AssetPanel}. */
export interface AssetPanelProps {
  readonly backendType: BackendType
  readonly category: Category
}

/** A panel containing the description and settings for an asset. */
export default function AssetPanel(props: AssetPanelProps) {
  const { backendType, category } = props
  const contextPropsRaw = useAssetPanelProps()
  const contextProps = backendType === contextPropsRaw?.backend?.type ? contextPropsRaw : null
  const { backend, item } = contextProps ?? {}
  const isReadonly = category.type === 'trash'
  const isCloud = backend?.type === BackendType.remote
  const isVisible = useIsAssetPanelVisible()

  const { getText } = useText()
  const { localStorage } = useLocalStorage()
  const [initialized, setInitialized] = useState(false)
  const initializedRef = useRef(initialized)
  initializedRef.current = initialized
  const [tabRaw, setTab] = useState(() => localStorage.get('assetPanelTab') ?? 'settings')
  const tab = (() => {
    if (!isCloud) {
      return 'settings'
    } else if (
      (item?.type === AssetType.secret || item?.type === AssetType.directory) &&
      tabRaw === 'versions'
    ) {
      return 'settings'
    } else if (item?.type !== AssetType.project && tabRaw === 'sessions') {
      return 'settings'
    } else {
      return tabRaw
    }
  })()

  useEffect(() => {
    // This prevents secrets and directories always setting the tab to `properties`
    // (because they do not support the `versions` tab).
    if (initializedRef.current) {
      localStorage.set('assetPanelTab', tabRaw)
    }
  }, [tabRaw, localStorage])

  useEffect(() => {
    setInitialized(true)
  }, [])

  return (
    <div
      className={twMerge(
        'flex flex-col overflow-hidden transition-min-width duration-side-panel ease-in-out',
        isVisible ? 'min-w-side-panel' : 'min-w-0',
      )}
      onClick={(event) => {
        // Prevent deselecting Assets Table rows.
        event.stopPropagation()
      }}
    >
      <Tabs
        data-testid="asset-panel"
        className={twMerge(
          'absolute flex h-full w-asset-panel flex-col bg-invert transition-[box-shadow] clip-path-left-shadow',
          isVisible ? 'shadow-softer' : '',
        )}
        selectedKey={tab}
        onSelectionChange={(newPage) => {
          const validated = TABS_SCHEMA.safeParse(newPage)
          if (validated.success) {
            setTab(validated.data)
          }
        }}
      >
        {item == null || backend == null ?
          <div className="grid grow place-items-center text-lg">
            {getText('selectExactlyOneAssetToViewItsDetails')}
          </div>
        : <>
            <div className="h-4 bg-primary/5" />
            <TabBar className="grow-0">
              <Tab id="settings" labelId="settings" isActive={tab === 'settings'} icon={null}>
                {getText('settings')}
              </Tab>
              {isCloud && item.type !== AssetType.secret && item.type !== AssetType.directory && (
                <Tab id="versions" labelId="versions" isActive={tab === 'versions'} icon={null}>
                  {getText('versions')}
                </Tab>
              )}
              {isCloud && item.type === AssetType.project && (
                <Tab
                  id="sessions"
                  labelId="projectSessions"
                  isActive={tab === 'sessions'}
                  icon={null}
                >
                  {getText('projectSessions')}
                </Tab>
              )}
            </TabBar>
            <TabPanel id="settings" className="p-4 pl-asset-panel-l">
              <AssetProperties
                key={item.item.id}
                backend={backend}
                isReadonly={isReadonly}
                item={item}
                category={category}
                spotlightOn={contextProps?.spotlightOn}
              />
            </TabPanel>
            <TabPanel id="versions" className="p-4 pl-asset-panel-l">
              <AssetVersions backend={backend} item={item} />
            </TabPanel>
            {item.type === AssetType.project && (
              <TabPanel id="sessions" className="p-4 pl-asset-panel-l">
                <ProjectSessions backend={backend} item={item} />
              </TabPanel>
            )}
          </>
        }
      </Tabs>
    </div>
  )
}
