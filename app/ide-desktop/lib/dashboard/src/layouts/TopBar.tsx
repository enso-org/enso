/** @file The top-bar of dashboard. */
import * as React from 'react'

import * as backendProvider from '#/providers/BackendProvider'

import type * as pageSwitcher from '#/layouts/PageSwitcher'
import PageSwitcher from '#/layouts/PageSwitcher'
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
  readonly setIsHelpChatOpen: (isHelpChatOpen: boolean) => void
  readonly doRemoveSelf: () => void
  readonly onSignOut: () => void
}

/** The {@link TopBarProps.setQuery} parameter is used to communicate with the parent component,
 * because `searchVal` may change parent component's project list. */
export default function TopBar(props: TopBarProps) {
  const { page, setPage, projectAsset, setProjectAsset, isEditorDisabled } = props
  const { setIsHelpChatOpen, doRemoveSelf, onSignOut } = props
  const remoteBackend = backendProvider.useRemoteBackend()

  return (
    <div className="flex">
      <PageSwitcher page={page} setPage={setPage} isEditorDisabled={isEditorDisabled} />
      <UserBar
        backend={remoteBackend}
        page={page}
        setPage={setPage}
        setIsHelpChatOpen={setIsHelpChatOpen}
        projectAsset={projectAsset}
        setProjectAsset={setProjectAsset}
        doRemoveSelf={doRemoveSelf}
        onSignOut={onSignOut}
      />
    </div>
  )
}
