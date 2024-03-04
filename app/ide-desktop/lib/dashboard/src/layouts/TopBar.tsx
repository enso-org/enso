/** @file The top-bar of dashboard. */
import * as React from 'react'

import type * as assetSearchBar from '#/layouts/AssetSearchBar'
import AssetSearchBar from '#/layouts/AssetSearchBar'
import BackendSwitcher from '#/layouts/BackendSwitcher'
import PageSwitcher, * as pageSwitcher from '#/layouts/PageSwitcher'
import UserBar from '#/layouts/UserBar'

import AssetInfoBar from '#/components/dashboard/AssetInfoBar'

import type * as backendModule from '#/services/Backend'

import type AssetQuery from '#/utilities/AssetQuery'

// ==============
// === TopBar ===
// ==============

/** Props for a {@link TopBar}. */
export interface TopBarProps {
  /** Whether the application may have the local backend running. */
  readonly supportsLocalBackend: boolean
  readonly isCloud: boolean
  readonly page: pageSwitcher.Page
  readonly setPage: (page: pageSwitcher.Page) => void
  readonly projectAsset: backendModule.ProjectAsset | null
  readonly setProjectAsset: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>> | null
  readonly isEditorDisabled: boolean
  readonly setBackendType: (backendType: backendModule.BackendType) => void
  readonly isHelpChatOpen: boolean
  readonly setIsHelpChatOpen: (isHelpChatOpen: boolean) => void
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly labels: backendModule.Label[]
  readonly suggestions: assetSearchBar.Suggestion[]
  readonly isAssetPanelVisible: boolean
  readonly setIsAssetPanelVisible: React.Dispatch<React.SetStateAction<boolean>>
  readonly doRemoveSelf: () => void
  readonly onSignOut: () => void
}

/** The {@link TopBarProps.setQuery} parameter is used to communicate with the parent component,
 * because `searchVal` may change parent component's project list. */
export default function TopBar(props: TopBarProps) {
  const { supportsLocalBackend, isCloud, page, setPage, projectAsset, setProjectAsset } = props
  const { isEditorDisabled, setBackendType, isHelpChatOpen, setIsHelpChatOpen } = props
  const { query, setQuery, labels, suggestions } = props
  const { isAssetPanelVisible, setIsAssetPanelVisible, doRemoveSelf, onSignOut } = props

  return (
    <div className="relative flex m-top-bar mb h-row gap-top-bar z-3">
      <PageSwitcher page={page} setPage={setPage} isEditorDisabled={isEditorDisabled} />
      {supportsLocalBackend && page !== pageSwitcher.Page.editor && (
        <BackendSwitcher setBackendType={setBackendType} />
      )}
      {page === pageSwitcher.Page.editor ? (
        <div className="flex-1" />
      ) : (
        <div className="flex-1 flex flex-wrap justify-around">
          <AssetSearchBar
            isCloud={isCloud}
            query={query}
            setQuery={setQuery}
            labels={labels}
            suggestions={suggestions}
          />
        </div>
      )}
      {!isAssetPanelVisible && (
        <div className="flex gap-top-bar-right">
          {page === pageSwitcher.Page.drive && (
            <AssetInfoBar
              isAssetPanelVisible={isAssetPanelVisible}
              setIsAssetPanelVisible={setIsAssetPanelVisible}
            />
          )}
          <UserBar
            supportsLocalBackend={supportsLocalBackend}
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
