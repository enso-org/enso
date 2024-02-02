/** @file A panel containing the description and settings for an asset. */
import * as React from 'react'

import * as localStorageProvider from '#/providers/LocalStorageProvider'

import type * as assetEvent from '#/events/assetEvent'

import AssetProperties from '#/layouts/dashboard/AssetProperties'
import AssetVersions from '#/layouts/dashboard/AssetVersions'
import type Category from '#/layouts/dashboard/CategorySwitcher/Category'
import type * as pageSwitcher from '#/layouts/dashboard/PageSwitcher'
import UserBar from '#/layouts/dashboard/UserBar'

import AssetInfoBar from '#/components/dashboard/AssetInfoBar'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import * as array from '#/utilities/array'
import type AssetTreeNode from '#/utilities/AssetTreeNode'
import LocalStorage from '#/utilities/LocalStorage'

// =====================
// === AssetPanelTab ===
// =====================

/** Determines the content of the {@link AssetPanel}. */
enum AssetPanelTab {
  properties = 'properties',
  versions = 'versions',
}

// ============================
// === Global configuration ===
// ============================

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    assetPanelTab: AssetPanelTab
  }
}

const TABS = Object.values(AssetPanelTab)
LocalStorage.registerKey('assetPanelTab', {
  tryParse: value => (array.includes(TABS, value) ? value : null),
})

// ==================
// === AssetPanel ===
// ==================

/** The subset of {@link AssetPanelProps} that are required to be supplied by the row. */
export interface AssetPanelRequiredProps {
  item: AssetTreeNode
  setItem: React.Dispatch<React.SetStateAction<AssetTreeNode>>
}

/** Props for an {@link AssetPanel}. */
export interface AssetPanelProps extends AssetPanelRequiredProps {
  supportsLocalBackend: boolean
  backend: Backend
  page: pageSwitcher.Page
  setPage: (page: pageSwitcher.Page) => void
  category: Category
  isHelpChatOpen: boolean
  setIsHelpChatOpen: React.Dispatch<React.SetStateAction<boolean>>
  setVisibility: React.Dispatch<React.SetStateAction<boolean>>
  dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
  projectAsset: backendModule.SmartProject | null
  setProjectAsset: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>> | null
  doRemoveSelf: () => void
  onSignOut: () => void
}

/** A panel containing the description and settings for an asset. */
export default function AssetPanel(props: AssetPanelProps) {
  const { supportsLocalBackend, backend, item, setItem, page, setPage, category } = props
  const { isHelpChatOpen, setIsHelpChatOpen, setVisibility } = props
  const { dispatchAssetEvent, projectAsset, setProjectAsset, doRemoveSelf, onSignOut } = props

  const { localStorage } = localStorageProvider.useLocalStorage()
  const [initialized, setInitialized] = React.useState(false)
  const [tab, setTab] = React.useState(() => {
    const savedTab = localStorage.get('assetPanelTab') ?? AssetPanelTab.properties
    if (
      (item.item.type === backendModule.AssetType.secret ||
        item.item.type === backendModule.AssetType.directory) &&
      savedTab === AssetPanelTab.versions
    ) {
      return AssetPanelTab.properties
    } else {
      return savedTab
    }
  })
  const isCloud = backend.type === backendModule.BackendType.remote

  React.useEffect(() => {
    // This prevents secrets and directories always setting the tab to `properties`
    // (because they do not support the `versions` tab).
    if (initialized) {
      localStorage.set('assetPanelTab', tab)
    }
    // `initialized` is NOT a dependency.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [tab, /* should never change */ localStorage])

  React.useEffect(() => {
    setInitialized(true)
  }, [])

  return (
    <div
      data-testid="asset-panel"
      className="absolute flex flex-col h-full border-black/[0.12] border-l-2 gap-8 w-120 pl-3 pr-4 py-2.25"
      onClick={event => {
        event.stopPropagation()
      }}
    >
      <div className="flex">
        {item.item.type !== backendModule.AssetType.secret &&
          item.item.type !== backendModule.AssetType.directory && (
            <button
              className={`rounded-full leading-5 px-2 select-none bg-frame hover:bg-frame-selected transition-colors ${
                tab !== AssetPanelTab.versions ? '' : 'bg-frame-selected'
              }`}
              onClick={() => {
                setTab(oldTab =>
                  oldTab === AssetPanelTab.versions
                    ? AssetPanelTab.properties
                    : AssetPanelTab.versions
                )
              }}
            >
              Versions
            </button>
          )}
        {/* Spacing. */}
        <div className="grow" />
        <div className="flex gap-2">
          <AssetInfoBar
            canToggleAssetPanel={true}
            isAssetPanelVisible={true}
            setIsAssetPanelVisible={setVisibility}
            isCloud={isCloud}
          />
          <UserBar
            supportsLocalBackend={supportsLocalBackend}
            backend={backend}
            isHelpChatOpen={isHelpChatOpen}
            setIsHelpChatOpen={setIsHelpChatOpen}
            onSignOut={onSignOut}
            page={page}
            setPage={setPage}
            projectAsset={projectAsset}
            setProjectAsset={setProjectAsset}
            doRemoveSelf={doRemoveSelf}
          />
        </div>
      </div>
      {tab === AssetPanelTab.properties && (
        <AssetProperties
          item={item}
          setItem={setItem}
          backend={backend}
          category={category}
          dispatchAssetEvent={dispatchAssetEvent}
        />
      )}
      <AssetVersions hidden={tab !== AssetPanelTab.versions} item={item} />
    </div>
  )
}
