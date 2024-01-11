/** @file The top-bar of dashboard. */
import * as React from 'react'

import type * as assetSearchBar from '#/layouts/dashboard/assetSearchBar'
import AssetSearchBar from '#/layouts/dashboard/assetSearchBar'
import BackendSwitcher from '#/layouts/dashboard/BackendSwitcher'
import PageSwitcher, * as pageSwitcher from '#/layouts/dashboard/PageSwitcher'
import UserBar from '#/layouts/dashboard/UserBar'
import type * as backendModule from '#/services/backend'
import type * as assetQuery from '#/utilities/assetQuery'

import AssetInfoBar from '#/components/dashboard/AssetInfoBar'

// ==============
// === TopBar ===
// ==============

/** Props for a {@link TopBar}. */
export interface TopBarProps {
    /** Whether the application may have the local backend running. */
    supportsLocalBackend: boolean
    page: pageSwitcher.Page
    setPage: (page: pageSwitcher.Page) => void
    projectAsset: backendModule.ProjectAsset | null
    setProjectAsset: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>> | null
    isEditorDisabled: boolean
    setBackendType: (backendType: backendModule.BackendType) => void
    isHelpChatOpen: boolean
    setIsHelpChatOpen: (isHelpChatOpen: boolean) => void
    query: assetQuery.AssetQuery
    setQuery: React.Dispatch<React.SetStateAction<assetQuery.AssetQuery>>
    labels: backendModule.Label[]
    suggestions: assetSearchBar.Suggestion[]
    canToggleSettingsPanel: boolean
    isSettingsPanelVisible: boolean
    setIsSettingsPanelVisible: React.Dispatch<React.SetStateAction<boolean>>
    doRemoveSelf: () => void
    onSignOut: () => void
}

/** The {@link TopBarProps.setQuery} parameter is used to communicate with the parent component,
 * because `searchVal` may change parent component's project list. */
export default function TopBar(props: TopBarProps) {
    const { supportsLocalBackend, page, setPage, projectAsset, setProjectAsset } = props
    const { isEditorDisabled, setBackendType, isHelpChatOpen, setIsHelpChatOpen } = props
    const { query, setQuery, labels, suggestions, canToggleSettingsPanel } = props
    const { isSettingsPanelVisible, setIsSettingsPanelVisible, doRemoveSelf, onSignOut } = props

    return (
        <div
            className={`relative flex ml-4.75 mr-2.25 h-8 gap-6 z-3 ${
                page !== pageSwitcher.Page.home ? 'mt-2.25' : 'my-2.25'
            }`}
        >
            <PageSwitcher page={page} setPage={setPage} isEditorDisabled={isEditorDisabled} />
            {supportsLocalBackend && page !== pageSwitcher.Page.editor && (
                <BackendSwitcher setBackendType={setBackendType} />
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
            {!isSettingsPanelVisible && (
                <div className="flex gap-2">
                    <AssetInfoBar
                        canToggleSettingsPanel={canToggleSettingsPanel}
                        isSettingsPanelVisible={isSettingsPanelVisible}
                        setIsSettingsPanelVisible={setIsSettingsPanelVisible}
                    />
                    <UserBar
                        supportsLocalBackend={supportsLocalBackend}
                        page={page}
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
