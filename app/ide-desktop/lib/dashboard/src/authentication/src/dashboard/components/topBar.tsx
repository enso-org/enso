/** @file The top-bar of dashboard. */
import * as React from 'react'

import BarsIcon from 'enso-assets/bars.svg'
import CloudIcon from 'enso-assets/cloud.svg'
import ComputerIcon from 'enso-assets/computer.svg'
import DefaultUserIcon from 'enso-assets/default_user.svg'
import MagnifyingGlassIcon from 'enso-assets/magnifying_glass.svg'
import SpeechBubbleIcon from 'enso-assets/speech_bubble.svg'

import * as backendModule from '../backend'
import * as dashboard from './dashboard'

import * as backendProvider from '../../providers/backend'
import * as modalProvider from '../../providers/modal'

import UserMenu from './userMenu'

// =================
// === Constants ===
// =================

/** A {@link RegExp} that matches {@link KeyboardEvent.code}s corresponding to non-printable
 * keys. */
const SPECIAL_CHARACTER_KEYCODE_REGEX = /^[A-Z][a-z]/

// ==============
// === TopBar ===
// ==============

/** Props for a {@link TopBar}. */
export interface TopBarProps {
    /** Whether the application may have the local backend running. */
    supportsLocalBackend: boolean
    projectName: string | null
    tab: dashboard.Tab
    toggleTab: () => void
    setBackendType: (backendType: backendModule.BackendType) => void
    query: string
    setQuery: (value: string) => void
}

/** The {@link TopBarProps.setQuery} parameter is used to communicate with the parent component,
 * because `searchVal` may change parent component's project list. */
function TopBar(props: TopBarProps) {
    const { supportsLocalBackend, projectName, tab, toggleTab, setBackendType, query, setQuery } =
        props
    const [isUserMenuVisible, setIsUserMenuVisible] = React.useState(false)
    const { modal } = modalProvider.useModal()
    const { setModal, unsetModal } = modalProvider.useSetModal()
    const { backend } = backendProvider.useBackend()
    // This is INCORRECT, but SAFE, as its value is guaranteed to be set by the time any hooks run.
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const searchRef = React.useRef<HTMLInputElement>(null!)

    React.useEffect(() => {
        if (!modal) {
            setIsUserMenuVisible(false)
        }
    }, [modal])

    React.useEffect(() => {
        if (isUserMenuVisible) {
            setModal(() => <UserMenu />)
        } else {
            unsetModal()
        }
    }, [isUserMenuVisible, setModal, unsetModal])

    React.useEffect(() => {
        const onKeyPress = (event: KeyboardEvent) => {
            // Allow `alt` key to be pressed in case it is being used to enter special characters.
            if (
                !event.ctrlKey &&
                !event.shiftKey &&
                !event.metaKey &&
                !SPECIAL_CHARACTER_KEYCODE_REGEX.test(event.key)
            ) {
                searchRef.current.focus()
            }
        }
        document.addEventListener('keypress', onKeyPress)
        return () => {
            document.removeEventListener('keypress', onKeyPress)
        }
    }, [])

    return (
        <div className="flex overflow-x-hidden h-8 mx-2">
            {supportsLocalBackend && (
                <div className="flex flex-nowrap shrink-0 bg-label rounded-full p-1.5">
                    <button
                        disabled={backend.type === backendModule.BackendType.local}
                        className={`${
                            backend.type === backendModule.BackendType.local
                                ? 'bg-white shadow-soft'
                                : 'opacity-50'
                        } rounded-full px-1.5 py-1`}
                        onClick={() => {
                            setBackendType(backendModule.BackendType.local)
                        }}
                    >
                        <img src={ComputerIcon} />
                    </button>
                    <button
                        disabled={backend.type === backendModule.BackendType.remote}
                        className={`${
                            backend.type === backendModule.BackendType.remote
                                ? 'bg-white shadow-soft'
                                : 'opacity-50'
                        } rounded-full px-1.5 py-1`}
                        onClick={() => {
                            setBackendType(backendModule.BackendType.remote)
                        }}
                    >
                        <img src={CloudIcon} />
                    </button>
                </div>
            )}
            <div
                className={`flex items-center shrink-0 bg-label rounded-full px-1 mx-2 w-39 ${
                    projectName != null ? 'cursor-pointer' : 'opacity-50'
                }`}
                onClick={toggleTab}
            >
                <span
                    className={`flex-1 opacity-50 overflow-hidden transition-width whitespace-nowrap ${
                        tab === dashboard.Tab.dashboard ? 'm-2' : 'grow-0 w-0'
                    }`}
                >
                    {projectName ?? 'Dashboard'}
                </span>
                <div className="bg-white shadow-soft rounded-full px-1.5 py-1">
                    <img src={BarsIcon} />
                </div>
                <span
                    className={`flex-1 opacity-50 overflow-hidden transition-width whitespace-nowrap leading-5 py-0.5 ${
                        tab === dashboard.Tab.ide ? 'm-2' : 'grow-0 w-0'
                    }`}
                >
                    {projectName ?? 'No project open'}
                </span>
            </div>
            <div className="flex flex-nowrap flex-initial bg-label items-center rounded-full gap-2.5 min-w-30 pl-2.75 pr-3.25 mx-2">
                <img src={MagnifyingGlassIcon} />
                <input
                    ref={searchRef}
                    type="text"
                    size={1}
                    placeholder="Click here or start typing to search for projects, data connectors, users, and more ..."
                    value={query}
                    onKeyDown={event => {
                        if (event.key === 'Escape') {
                            event.stopPropagation()
                            event.currentTarget.blur()
                            setQuery('')
                            event.currentTarget.value = ''
                        } else if (event.key === 'Enter') {
                            event.stopPropagation()
                            event.currentTarget.blur()
                        }
                    }}
                    onChange={event => {
                        setQuery(event.target.value)
                    }}
                    className="bg-transparent w-118.25 h-5 py-0.5"
                />
            </div>
            {/* Padding. */}
            <div className="grow" />
            <a
                href="https://discord.gg/enso"
                target="_blank"
                rel="noreferrer"
                className="flex items-center shrink-0 text-white bg-help rounded-full gap-2 pl-2.5 pr-2.25 mx-1.25"
            >
                <span className="whitespace-nowrap my-0.5">help chat</span>
                <img src={SpeechBubbleIcon} />
            </a>
            {/* User profile and menu. */}
            <div className="transform w-8 mx-2.75">
                <div
                    onClick={event => {
                        event.stopPropagation()
                        setIsUserMenuVisible(!isUserMenuVisible)
                    }}
                    className="rounded-full w-8 h-8 bg-cover cursor-pointer"
                >
                    <img src={DefaultUserIcon} />
                </div>
            </div>
        </div>
    )
}

export default TopBar
