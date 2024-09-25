/** @file A panel containing the description and settings for an asset. */
import { type Dispatch, type SetStateAction, useEffect, useRef, useState } from 'react'

import * as z from 'zod'

import { TabPanel, Tabs } from '#/components/aria'
import AssetProperties from '#/layouts/AssetPanel/AssetProperties'
import ProjectExecutions from '#/layouts/AssetPanel/ProjectExecutions'
import ProjectSessions from '#/layouts/AssetPanel/ProjectSessions'
import AssetVersions from '#/layouts/AssetVersions/AssetVersions'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import { TabBar } from '#/layouts/TabBar'
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

const ASSET_PANEL_TABS = ['information', 'versions', 'sessions', 'schedules'] as const
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
  readonly setItem: Dispatch<SetStateAction<AnyAssetTreeNode>> | null
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
  const { backend, item, setItem } = contextProps ?? {}
  const isReadonly = category.type === 'trash'
  const isCloud = backend?.type === BackendType.remote
  const isVisible = useIsAssetPanelVisible()

  const { getText } = useText()
  const { localStorage } = useLocalStorage()
  const [initialized, setInitialized] = useState(false)
  const initializedRef = useRef(initialized)
  initializedRef.current = initialized
  const [tabRaw, setTab] = useState(() => localStorage.get('assetPanelTab') ?? 'information')
  const tab = (() => {
    if (!isCloud) {
      return 'information'
    } else if (
      (item?.item.type === AssetType.secret || item?.item.type === AssetType.directory) &&
      tabRaw === 'versions'
    ) {
      return 'information'
    } else if (item?.item.type !== AssetType.project && tabRaw === 'sessions') {
      return 'information'
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
        {item == null || setItem == null || backend == null ?
          <div className="grid grow place-items-center text-lg">
            {getText('selectExactlyOneAssetToViewItsDetails')}
          </div>
        : <>
            <div className="h-4 bg-primary/5" />
            <TabBar className="grow-0">
              <TabBar.Tab
                id="information"
                labelId="information"
                isActive={tab === 'information'}
                icon={null}
              >
                {getText('information')}
              </TabBar.Tab>
              {isCloud &&
                item.item.type !== AssetType.secret &&
                item.item.type !== AssetType.directory && (
                  <TabBar.Tab
                    id="versions"
                    labelId="versions"
                    isActive={tab === 'versions'}
                    icon={null}
                  >
                    {getText('versions')}
                  </TabBar.Tab>
                )}
              {isCloud && item.item.type === AssetType.project && (
                <TabBar.Tab
                  id="sessions"
                  labelId="projectSessions"
                  isActive={tab === 'sessions'}
                  icon={null}
                >
                  {getText('projectSessions')}
                </TabBar.Tab>
              )}
              {isCloud && item.item.type === AssetType.project && (
                <TabBar.Tab
                  id="schedules"
                  labelId="executions"
                  isActive={tab === 'schedules'}
                  icon={null}
                >
                  {getText('executions')}
                </TabBar.Tab>
              )}
            </TabBar>
            <TabPanel id="information" className="p-4 pl-asset-panel-l">
              <AssetProperties
                key={item.item.id}
                backend={backend}
                isReadonly={isReadonly}
                item={item}
                setItem={setItem}
                category={category}
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
            {item.type === AssetType.project && (
              <TabPanel id="schedules" className="p-4 pl-asset-panel-l">
                <ProjectExecutions backend={backend} item={item.item} />
              </TabPanel>
            )}
          </>
        }
      </Tabs>
    </div>
  )
}
