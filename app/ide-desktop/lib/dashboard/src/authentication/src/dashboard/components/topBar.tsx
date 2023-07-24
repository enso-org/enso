/** @file The top-bar of dashboard. */
import * as React from 'react'

import BarsIcon from 'enso-assets/bars.svg'
import MagnifyingGlassIcon from 'enso-assets/magnifying_glass.svg'

import * as backendModule from '../backend'
import * as tabModule from '../tab'

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
    tab: tabModule.Tab
    toggleTab: () => void
    setBackendType: (backendType: backendModule.BackendType) => void
    isHelpChatOpen: boolean
    setIsHelpChatOpen: (isHelpChatOpen: boolean) => void
    query: string
    setQuery: (value: string) => void
}

/** The {@link TopBarProps.setQuery} parameter is used to communicate with the parent component,
 * because `searchVal` may change parent component's project list. */
function TopBar(props: TopBarProps) {
    const {
        supportsLocalBackend,
        projectName,
        tab,
        toggleTab,
        setBackendType,
        isHelpChatOpen,
        setIsHelpChatOpen,
        query,
        setQuery,
    } = props

    return (
        <div className="flex mx-4.75 h-8 gap-6">
            {supportsLocalBackend || <BackendSwitcher setBackendType={setBackendType} />}
            <div
                className={`flex items-center bg-label rounded-full pl-1 pr-2.5 mx-2 ${
                    projectName != null ? 'cursor-pointer' : 'opacity-50'
                }`}
                onClick={toggleTab}
            >
                <span
                    className={`opacity-50 overflow-hidden transition-width nowrap ${
                        tab === tabModule.Tab.dashboard ? 'm-2 w-16' : 'w-0'
                    }`}
                >
                    {projectName ?? 'Dashboard'}
                </span>
                <div className="bg-white shadow-soft rounded-full px-1.5 py-1">
                    <img src={BarsIcon} />
                </div>
                <span
                    className={`opacity-50 overflow-hidden transition-width nowrap ${
                        tab === tabModule.Tab.ide ? 'm-2 w-16' : 'w-0'
                    }`}
                >
                    {projectName ?? 'No project open'}
                </span>
            </div>
            <div className="grow flex items-center bg-label rounded-full px-2">
                <div>
                    <img src={MagnifyingGlassIcon} />
                </div>
                <input
                    type="text"
                    size={1}
                    placeholder="Click here or start typing to search for projects, data connectors, users, and more ..."
                    value={query}
                    onChange={event => {
                        setQuery(event.target.value)
                    }}
                    className="flex-1 mx-2 bg-transparent"
                />
            </div>
            <div className="grow" />
            <UserBar isHelpChatOpen={isHelpChatOpen} setIsHelpChatOpen={setIsHelpChatOpen} />
        </div>
    )
}

export default TopBar
