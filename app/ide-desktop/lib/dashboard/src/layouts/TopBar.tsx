/** @file The top-bar of dashboard. */
import * as React from 'react'

import * as backendProvider from '#/providers/BackendProvider'

import PageSwitcher, * as pageSwitcher from '#/layouts/PageSwitcher'
import UserBar from '#/layouts/UserBar'

import type * as backendModule from '#/services/Backend'

// ==============
// === TopBar ===
// ==============

/** Props for a {@link TopBar}. */
export interface TopBarProps {
  readonly page: pageSwitcher.Page
  readonly setPage: (page: pageSwitcher.Page) => void
  readonly projectAsset: backendModule.ProjectAsset | null
  readonly setProjectAsset: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>> | null
  readonly isEditorDisabled: boolean
  readonly isHelpChatOpen: boolean
  readonly setIsHelpChatOpen: (isHelpChatOpen: boolean) => void
  readonly isAssetPanelVisible: boolean
  readonly setIsAssetPanelEnabled: React.Dispatch<React.SetStateAction<boolean>>
  readonly doRemoveSelf: () => void
  readonly onSignOut: () => void
}

/** The {@link TopBarProps.setQuery} parameter is used to communicate with the parent component,
 * because `searchVal` may change parent component's project list. */
export default function TopBar(props: TopBarProps) {
  const { page, setPage, projectAsset, setProjectAsset } = props
  const { isEditorDisabled, isHelpChatOpen, setIsHelpChatOpen } = props
  const { isAssetPanelVisible, doRemoveSelf, onSignOut } = props
  const remoteBackend = backendProvider.useRemoteBackend()
  const shouldMakeSpaceForExtendedEditorMenu = page === pageSwitcher.Page.editor

  return (
    <div className="p-top-bar h-12 bg-primary/5 pb">
      <div className="relative z-1 flex gap-top-bar">
        <PageSwitcher page={page} setPage={setPage} isEditorDisabled={isEditorDisabled} />
        <div
          className={`grid transition-all duration-side-panel ${isAssetPanelVisible ? 'grid-cols-0fr' : 'grid-cols-1fr'}`}
        >
          <div className="invisible overflow-hidden pointer-events-none-recursive">
            <UserBar
              invisible
              backend={remoteBackend}
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
        <div
          className={`fixed top z-1 m-top-bar text-xs text-primary transition-all duration-side-panel ${shouldMakeSpaceForExtendedEditorMenu ? 'mr-extended-editor-menu' : ''} ${isAssetPanelVisible ? '-right-asset-panel-w' : 'right'}`}
        >
          <UserBar
            backend={remoteBackend}
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
