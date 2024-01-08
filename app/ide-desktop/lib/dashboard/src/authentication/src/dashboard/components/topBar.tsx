/** @file The top-bar of dashboard. */
import * as React from 'react'

import type * as assetQuery from '../../assetQuery'
import type * as backendModule from '../backend'

import type * as assetSearchBar from './assetSearchBar'
import PageSwitcher, * as pageSwitcher from './pageSwitcher'
import AssetInfoBar from './assetInfoBar'
import AssetSearchBar from './assetSearchBar'
import BackendSwitcher from './backendSwitcher'
import UserBar from './userBar'

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
    const {
        supportsLocalBackend,
        page,
        setPage,
        projectAsset,
        setProjectAsset,
        isEditorDisabled,
        setBackendType,
        isHelpChatOpen,
        setIsHelpChatOpen,
        query,
        setQuery,
        labels,
        suggestions,
        canToggleSettingsPanel,
        isSettingsPanelVisible,
        setIsSettingsPanelVisible,
        doRemoveSelf,
        onSignOut,
    } = props

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
            <div className="grow" />
            {page !== pageSwitcher.Page.editor && (
                <>
                    <AssetSearchBar
                        query={query}
                        setQuery={setQuery}
                        labels={labels}
                        suggestions={suggestions}
                    />
                    <div className="grow" />
                </>
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
