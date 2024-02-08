/** @file The top-bar of dashboard. */
import * as React from 'react'

import type * as assetSearchBar from '#/layouts/dashboard/AssetSearchBar'
import AssetSearchBar from '#/layouts/dashboard/AssetSearchBar'
import BackendSwitcher from '#/layouts/dashboard/BackendSwitcher'
import PageSwitcher, * as pageSwitcher from '#/layouts/dashboard/PageSwitcher'
import UserBar from '#/layouts/dashboard/UserBar'

import AssetInfoBar from '#/components/dashboard/AssetInfoBar'

import * as backendModule from '#/services/Backend'

import type AssetQuery from '#/utilities/AssetQuery'

// ==============
// === TopBar ===
// ==============

/** Props for a {@link TopBar}. */
export interface TopBarProps {
  /** Whether the application may have the local backend running. */
  readonly supportsLocalBackend: boolean
  readonly page: pageSwitcher.Page
  readonly setPage: (page: pageSwitcher.Page) => void
  readonly projectAsset: backendModule.SmartProject | null
  readonly setProjectAsset: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>> | null
  readonly isCloud: boolean
  readonly isEditorDisabled: boolean
  readonly backendType: backendModule.BackendType
  readonly setBackendType: (backendType: backendModule.BackendType) => void
  readonly isHelpChatOpen: boolean
  readonly setIsHelpChatOpen: (isHelpChatOpen: boolean) => void
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly labels: backendModule.Label[]
  readonly suggestions: assetSearchBar.Suggestion[]
  readonly canToggleAssetPanel: boolean
  readonly isAssetPanelVisible: boolean
  readonly setIsAssetPanelVisible: React.Dispatch<React.SetStateAction<boolean>>
  readonly doRemoveSelf: () => void
  readonly onSignOut: () => void
}

/** The {@link TopBarProps.setQuery} parameter is used to communicate with the parent component,
 * because `searchVal` may change parent component's project list. */
export default function TopBar(props: TopBarProps) {
  const { supportsLocalBackend, page, setPage, projectAsset, setProjectAsset } = props
  const { isEditorDisabled, backendType, setBackendType, isHelpChatOpen, setIsHelpChatOpen } = props
  const { query, setQuery, labels, suggestions, canToggleAssetPanel } = props
  const { isAssetPanelVisible, setIsAssetPanelVisible, doRemoveSelf, onSignOut } = props
  const isCloud = backendType === backendModule.BackendType.remote

  return (
    <div
      className={`relative flex ml-4.75 mr-2.25 h-8 gap-6 z-3 ${
        page !== pageSwitcher.Page.home ? 'mt-2.25' : 'my-2.25'
      }`}
    >
      <PageSwitcher page={page} setPage={setPage} isEditorDisabled={isEditorDisabled} />
      {supportsLocalBackend && page !== pageSwitcher.Page.editor && (
        <BackendSwitcher backendType={backendType} setBackendType={setBackendType} />
      )}
      {page === pageSwitcher.Page.editor ? (
        <div className="flex-1" />
      ) : (
        <div className="flex-1 flex flex-wrap justify-around">
          <AssetSearchBar
            query={query}
            setQuery={setQuery}
            labels={labels}
            suggestions={suggestions}
          />
        </div>
      )}
      {!isAssetPanelVisible && (
        <div className="flex gap-2">
          <AssetInfoBar
            canToggleAssetPanel={canToggleAssetPanel}
            isAssetPanelVisible={isAssetPanelVisible}
            setIsAssetPanelVisible={setIsAssetPanelVisible}
            isCloud={isCloud}
          />
          <UserBar
            supportsLocalBackend={supportsLocalBackend}
            isCloud={isCloud}
            page={page}
            setPage={setPage}
            isHelpChatOpen={isHelpChatOpen}
            setIsHelpChatOpen={setIsHelpChatOpen}
            projectAsset={projectAsset}
            setProjectAsset={setProjectAsset}
            doRemoveSelf={doRemoveSelf}
            onSignOut={onSignOut}
          />
        </div>
      )}
    </div>
  )
}
