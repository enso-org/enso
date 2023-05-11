/** @file The top-bar of dashboard. */
import * as react from 'react'

import * as dashboard from './dashboard'
import * as platformModule from '../../platform'
import * as svg from '../../components/svg'

import * as backendProvider from '../../providers/backend'
import * as modalProvider from '../../providers/modal'

import UserMenu from './userMenu'

// ==============
// === TopBar ===
// ==============

interface TopBarProps {
    platform: platformModule.Platform
    projectName: string | null
    tab: dashboard.Tab
    toggleTab: () => void
    setBackendPlatform: (backendPlatform: platformModule.Platform) => void
    query: string
    setQuery: (value: string) => void
}

/**
 * The {@link TopBarProps.setQuery} param is used to communicate with the parent component,
 * because `searchVal` may change parent component's project list.
 */
function TopBar(props: TopBarProps) {
    const { platform, projectName, tab, toggleTab, setBackendPlatform, query, setQuery } = props
    const [userMenuVisible, setUserMenuVisible] = react.useState(false)
    const { setModal, unsetModal } = modalProvider.useSetModal()
    const { backend } = backendProvider.useBackend()

    react.useEffect(() => {
        if (userMenuVisible) {
            setModal(() => <UserMenu />)
        } else {
            unsetModal()
        }
    }, [userMenuVisible])

    return (
        <div className="flex mb-2 h-8">
            <div className="flex text-primary">
                {platform === platformModule.Platform.desktop && (
                    <div className="bg-gray-100 rounded-full flex flex-row flex-nowrap p-1.5">
                        <button
                            onClick={() => {
                                setBackendPlatform(platformModule.Platform.desktop)
                            }}
                            className={`${
                                backend.platform === platformModule.Platform.desktop
                                    ? 'bg-white shadow-soft'
                                    : 'opacity-50'
                            } rounded-full px-1.5 py-1`}
                        >
                            {svg.COMPUTER_ICON}
                        </button>
                        <button
                            onClick={() => {
                                setBackendPlatform(platformModule.Platform.cloud)
                            }}
                            className={`${
                                backend.platform === platformModule.Platform.cloud
                                    ? 'bg-white shadow-soft'
                                    : 'opacity-50'
                            } rounded-full px-1.5 py-1`}
                        >
                            {svg.CLOUD_ICON}
                        </button>
                    </div>
                )}
                <div
                    className={`flex items-center bg-label rounded-full pl-1
                                pr-2.5 mx-2 ${projectName ? 'cursor-pointer' : 'opacity-50'}`}
                    onClick={toggleTab}
                >
                    <span
                        className={`opacity-50 overflow-hidden transition-width nowrap ${
                            tab === dashboard.Tab.dashboard ? 'm-2 w-16' : 'w-0'
                        }`}
                    >
                        {projectName ?? 'Dashboard'}
                    </span>
                    <div className="bg-white shadow-soft rounded-full px-1.5 py-1">
                        {svg.BARS_ICON}
                    </div>
                    <span
                        className={`opacity-50 overflow-hidden transition-width nowrap ${
                            tab === dashboard.Tab.ide ? 'm-2 w-16' : 'w-0'
                        }`}
                    >
                        {projectName ?? 'No project open'}
                    </span>
                </div>
                <div className="flex items-center bg-label rounded-full px-2 w-140 max-w-2xl">
                    <div>{svg.MAGNIFYING_GLASS_ICON}</div>
                    <input
                        type="text"
                        placeholder="Click here or start typing to search for projects, data connectors, users, and more ..."
                        value={query}
                        onChange={event => {
                            setQuery(event.target.value)
                        }}
                        className="flex-1 mx-2 bg-transparent"
                    />
                </div>
            </div>
            <div className="grow" />
            <a
                href="https://discord.gg/enso"
                target="_blank"
                className="flex items-center bg-help rounded-full px-2.5 text-white mx-2"
            >
                <span>help chat</span>
                <div className="ml-2">{svg.SPEECH_BUBBLE_ICON}</div>
            </a>
            {/* User profile and menu. */}
            <div className="transform">
                <div
                    onClick={event => {
                        event.stopPropagation()
                        setUserMenuVisible(!userMenuVisible)
                    }}
                    className="rounded-full w-8 h-8 bg-cover cursor-pointer"
                >
                    {svg.DEFAULT_USER_ICON}
                </div>
            </div>
        </div>
    )
}

export default TopBar
