/** @file A panel containing the description and settings for an asset. */
import * as React from 'react'

import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as assetEvent from '#/events/assetEvent'

import AssetProperties from '#/layouts/AssetProperties'
import AssetVersions from '#/layouts/AssetVersions/AssetVersions'
import type Category from '#/layouts/CategorySwitcher/Category'

import UnstyledButton from '#/components/UnstyledButton'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import * as array from '#/utilities/array'
import type * as assetTreeNode from '#/utilities/AssetTreeNode'
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
    readonly assetPanelTab: AssetPanelTab
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
  readonly backend: Backend | null
  readonly item: assetTreeNode.AnyAssetTreeNode | null
  readonly setItem: React.Dispatch<React.SetStateAction<assetTreeNode.AnyAssetTreeNode>> | null
}

/** Props for an {@link AssetPanel}. */
export interface AssetPanelProps extends AssetPanelRequiredProps {
  readonly isReadonly?: boolean
  readonly category: Category
  readonly labels: backendModule.Label[]
  readonly dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
}

/** A panel containing the description and settings for an asset. */
export default function AssetPanel(props: AssetPanelProps) {
  const { backend, item, setItem, category, labels, dispatchAssetEvent } = props
  const { isReadonly = false } = props

  const { getText } = textProvider.useText()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const [initialized, setInitialized] = React.useState(false)
  const [tab, setTab] = React.useState(() => {
    const savedTab = localStorage.get('assetPanelTab') ?? AssetPanelTab.properties
    if (
      (item?.item.type === backendModule.AssetType.secret ||
        item?.item.type === backendModule.AssetType.directory) &&
      savedTab === AssetPanelTab.versions
    ) {
      return AssetPanelTab.properties
    } else {
      return savedTab
    }
  })

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
      className="p-top-bar-margin pointer-events-none absolute flex h-full w-asset-panel flex-col gap-asset-panel bg-white pl-asset-panel-l"
      onClick={event => {
        event.stopPropagation()
      }}
    >
      <div className="flex py-2">
        {item != null &&
          item.item.type !== backendModule.AssetType.secret &&
          item.item.type !== backendModule.AssetType.directory && (
            <UnstyledButton
              className={`button pointer-events-auto h-8 select-none bg-frame px-button-x leading-cozy transition-colors hover:bg-primary/[8%] ${
                tab !== AssetPanelTab.versions ? '' : 'bg-primary/[8%] active'
              }`}
              onPress={() => {
                setTab(oldTab =>
                  oldTab === AssetPanelTab.versions
                    ? AssetPanelTab.properties
                    : AssetPanelTab.versions
                )
              }}
            >
              {getText('versions')}
            </UnstyledButton>
          )}
        {/* Spacing. The top right asset and user bars overlap this area. */}
        <div className="grow" />
      </div>
      {item == null || setItem == null || backend == null ? (
        <div className="grid grow place-items-center text-lg">
          {getText('selectExactlyOneAssetToViewItsDetails')}
        </div>
      ) : (
        <>
          {tab === AssetPanelTab.properties && (
            <AssetProperties
              backend={backend}
              isReadonly={isReadonly}
              item={item}
              setItem={setItem}
              category={category}
              labels={labels}
              dispatchAssetEvent={dispatchAssetEvent}
            />
          )}
          {tab === AssetPanelTab.versions && <AssetVersions backend={backend} item={item} />}
        </>
      )}
    </div>
  )
}
