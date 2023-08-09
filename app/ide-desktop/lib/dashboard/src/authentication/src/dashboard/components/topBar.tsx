/** @file The top-bar of dashboard. */
import * as React from 'react'

import FindIcon from 'enso-assets/find.svg'

import * as backendModule from '../backend'

import PageSwitcher, * as pageSwitcher from './pageSwitcher'
import AssetInfoBar from './assetInfoBar'
import BackendSwitcher from './backendSwitcher'
import UserBar from './userBar'

// ==============
// === TopBar ===
// ==============

/** Props for a {@link TopBar}. */
export interface TopBarProps {
    /** Whether the application may have the local backend running. */
    supportsLocalBackend: boolean
    projectName: string | null
    page: pageSwitcher.Page
    setPage: (page: pageSwitcher.Page) => void
    asset: backendModule.Asset | null
    isEditorDisabled: boolean
    setBackendType: (backendType: backendModule.BackendType) => void
    isHelpChatOpen: boolean
    setIsHelpChatOpen: (isHelpChatOpen: boolean) => void
    query: string
    setQuery: (value: string) => void
    onSignOut: () => void
}

/** The {@link TopBarProps.setQuery} parameter is used to communicate with the parent component,
 * because `searchVal` may change parent component's project list. */
export default function TopBar(props: TopBarProps) {
    const {
        supportsLocalBackend,
        page,
        setPage,
        asset,
        isEditorDisabled,
        setBackendType,
        isHelpChatOpen,
        setIsHelpChatOpen,
        query,
        setQuery,
        onSignOut,
    } = props

    return (
        <div className="relative flex ml-4.75 mr-2.25 mt-2.25 h-8 gap-6 z-10">
            <PageSwitcher page={page} setPage={setPage} isEditorDisabled={isEditorDisabled} />
            {supportsLocalBackend && page !== pageSwitcher.Page.editor && (
                <BackendSwitcher setBackendType={setBackendType} />
            )}
            <div className="grow" />
            {page !== pageSwitcher.Page.editor && (
                <>
                    <div className="search-bar absolute flex items-center text-primary bg-frame-bg rounded-full -translate-x-1/2 gap-2.5 left-1/2 h-8 w-98.25 px-2">
                        <label htmlFor="search">
                            <img src={FindIcon} className="opacity-80" />
                        </label>
                        <input
                            type="text"
                            size={1}
                            id="search"
                            placeholder="Type to search for projects, data connectors, users, and more."
                            value={query}
                            onChange={event => {
                                setQuery(event.target.value)
                            }}
                            className="grow bg-transparent leading-5 h-6 py-px"
                        />
                    </div>
                    <div className="grow" />
                </>
            )}
            <div className="flex gap-2">
                <AssetInfoBar asset={asset} />
                <UserBar
                    isHelpChatOpen={isHelpChatOpen}
                    setIsHelpChatOpen={setIsHelpChatOpen}
                    onSignOut={onSignOut}
                />
            </div>
        </div>
    )
}
