/** @file The top-bar of dashboard. */
import * as React from 'react'

import FindIcon from 'enso-assets/find.svg'

import BackendSwitcher from '#/layouts/dashboard/backendSwitcher'
import PageSwitcher, * as pageSwitcher from '#/layouts/dashboard/pageSwitcher'
import UserBar from '#/layouts/dashboard/userBar'
import type * as backendModule from '#/services/backend'
import * as assetQuery from '#/util/assetQuery'
import * as shortcuts from '#/util/shortcuts'

import AssetInfoBar from '#/components/dashboard/assetInfoBar'

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
    setQuery: (query: assetQuery.AssetQuery) => void
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
        canToggleSettingsPanel,
        isSettingsPanelVisible,
        setIsSettingsPanelVisible,
        doRemoveSelf,
        onSignOut,
    } = props
    const searchRef = React.useRef<HTMLInputElement>(null)

    React.useEffect(() => {
        const onKeyDown = (event: KeyboardEvent) => {
            // Allow `alt` key to be pressed in case it is being used to enter special characters.
            if (
                !(event.target instanceof HTMLInputElement) &&
                (!(event.target instanceof HTMLElement) || !event.target.isContentEditable) &&
                shortcuts.isTextInputEvent(event)
            ) {
                searchRef.current?.focus()
            }
        }
        document.addEventListener('keydown', onKeyDown)
        return () => {
            document.removeEventListener('keydown', onKeyDown)
        }
    }, [])

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
                    <label className="search-bar absolute flex items-center text-primary bg-frame rounded-full -translate-x-1/2 gap-2.5 left-1/2 h-8 w-98.25 min-w-31.5 px-2">
                        <img src={FindIcon} className="opacity-80" />
                        <input
                            ref={searchRef}
                            type="text"
                            size={1}
                            placeholder="Type to search for projects, data connectors, users, and more."
                            value={query.query}
                            onChange={event => {
                                setQuery(assetQuery.AssetQuery.fromString(event.target.value))
                            }}
                            className="grow bg-transparent leading-5 h-6 py-px"
                        />
                    </label>
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
