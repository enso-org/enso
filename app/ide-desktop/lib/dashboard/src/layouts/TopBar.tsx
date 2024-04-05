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
  readonly projectAsset: backendModule.SmartProject | null
  readonly setProjectAsset: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>> | null
  readonly isEditorDisabled: boolean
  readonly backendType: backendModule.BackendType
  readonly setBackendType: (backendType: backendModule.BackendType) => void
  readonly isHelpChatOpen: boolean
  readonly setIsHelpChatOpen: (isHelpChatOpen: boolean) => void
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly labels: backendModule.Label[]
  readonly suggestions: assetSearchBar.Suggestion[]
  readonly isAssetPanelVisible: boolean
  readonly isAssetPanelEnabled: boolean
  readonly setIsAssetPanelEnabled: React.Dispatch<React.SetStateAction<boolean>>
  readonly doRemoveSelf: () => void
  readonly onSignOut: () => void
}

/** The {@link TopBarProps.setQuery} parameter is used to communicate with the parent component,
 * because `searchVal` may change parent component's project list. */
export default function TopBar(props: TopBarProps) {
  const { supportsLocalBackend, isCloud, page, setPage, projectAsset, setProjectAsset } = props
  const { isEditorDisabled, backendType, setBackendType, isHelpChatOpen, setIsHelpChatOpen } = props
  const { query, setQuery, labels, suggestions, isAssetPanelEnabled } = props
  const { isAssetPanelVisible, setIsAssetPanelEnabled, doRemoveSelf, onSignOut } = props
  const supportsCloudBackend = process.env.ENSO_CLOUD_API_URL != null
  const shouldMakeSpaceForExtendedEditorMenu = page === pageSwitcher.Page.editor

  return (
    <div
      className={`relative z-1 m-top-bar flex h-row gap-top-bar ${page === pageSwitcher.Page.home ? 'mb-top-bar' : 'mb'}`}
    >
      <PageSwitcher page={page} setPage={setPage} isEditorDisabled={isEditorDisabled} />
      {supportsLocalBackend && supportsCloudBackend && page !== pageSwitcher.Page.editor && (
        <BackendSwitcher backendType={backendType} setBackendType={setBackendType} />
      )}
      {page === pageSwitcher.Page.editor ? (
        <div className="flex-1" />
      ) : (
        <div className="flex flex-1 flex-wrap justify-around">
          <AssetSearchBar
            isCloud={isCloud}
            query={query}
            setQuery={setQuery}
            labels={labels}
            suggestions={suggestions}
          />
        </div>
      )}
      <div
        className={`grid transition-all duration-side-panel ${isAssetPanelVisible ? 'grid-cols-0fr' : 'grid-cols-1fr'}`}
      >
        <div className="invisible flex gap-top-bar-right overflow-hidden pointer-events-none-recursive">
          {page === pageSwitcher.Page.drive && (
            <AssetInfoBar
              invisible
              isAssetPanelEnabled={isAssetPanelEnabled}
              setIsAssetPanelEnabled={setIsAssetPanelEnabled}
              isCloud={isCloud}
            />
          )}
          {supportsCloudBackend && (
            <UserBar
              invisible
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
          )}
        </div>
      </div>
      <div
        className={`fixed top z-1 m-top-bar text-xs text-primary transition-all duration-side-panel ${shouldMakeSpaceForExtendedEditorMenu ? 'mr-extended-editor-menu' : ''} ${isAssetPanelVisible ? '-right-asset-panel-w' : 'right'}`}
      >
        <div className="flex gap-top-bar-right">
          {page === pageSwitcher.Page.drive && (
            <AssetInfoBar
              isCloud={isCloud}
              isAssetPanelEnabled={isAssetPanelEnabled}
              setIsAssetPanelEnabled={setIsAssetPanelEnabled}
            />
          )}
          <UserBar
            isCloud={isCloud}
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
      </div>
    </div>
  )
}
