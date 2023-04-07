/** @file The top-bar of dashboard. */
import * as react from 'react'

import * as dashboard from './dashboard'

import UserMenu from './userMenu'

// ==============
// === TopBar ===
// ==============

/** Icons constant using by TopBar. */
const ICONS = {
    bar: (
        <svg
            width="16"
            height="16"
            viewBox="0 0 16 16"
            fill="none"
            xmlns="http://www.w3.org/2000/svg"
        >
            <rect x="2" y="1" width="12" height="3" fill="#767676" />
            <rect x="2" y="6" width="12" height="3" fill="#767676" />
            <rect x="2" y="11" width="12" height="3" fill="#767676" />
        </svg>
    ),
    search: (
        <svg
            width="16"
            height="16"
            viewBox="0 0 16 16"
            fill="none"
            xmlns="http://www.w3.org/2000/svg"
        >
            <g opacity="0.5">
                <path
                    d="M11.4142 10L15.6569 14.2426L14.2426 15.6569L10 11.4142L11.4142 10Z"
                    fill="#3E515F"
                />
                <circle cx="7" cy="7" r="5" stroke="#3E515F" stroke-width="2" />
            </g>
        </svg>
    ),
    chat: (
        <svg
            width="16"
            height="17"
            viewBox="0 0 16 17"
            fill="none"
            xmlns="http://www.w3.org/2000/svg"
        >
            <g opacity="0.9">
                <ellipse cx="8" cy="8" rx="8" ry="7.5" fill="white" />
                <path d="M4.17269e-05 16.5L2 10.5L5.50006 14L4.17269e-05 16.5Z" fill="white" />
            </g>
        </svg>
    ),
}

interface TopBarProps {
    projectName: string | null
    tab: dashboard.Tab
    toggleTab: () => void
    searchVal: string
    setSearchVal: (searchVal: string) => void
}

/**
 * The {@link TopBarProps["bindSearchVal"]} params is used to communicate with the parent component.
 * Because searchVal maybe change parent component's project list.
 */
function TopBar(props: TopBarProps) {
    const { projectName, tab, toggleTab, searchVal, setSearchVal } = props

    const [isOpenMenu, setIsOpenMenu] = react.useState(false)

    return (
        <div className="flex justify-between m-2 text-[11.5px] h-8">
            <div className="flex ml-[80px] text-primary">
                <div
                    className={`flex items-center bg-label rounded-full pl-1
                                pr-2.5 mr-4 ${projectName ? 'cursor-pointer' : 'opacity-50'}`}
                    onClick={toggleTab}
                >
                    <span
                        className={`opacity-50 overflow-hidden transition-width nowrap ${
                            tab === dashboard.Tab.dashboard ? 'm-2 w-16' : 'w-0'
                        }`}
                    >
                        Dashboard
                    </span>
                    <div className="bg-white shadow-soft px-1.5 py-1 rounded-full">{ICONS.bar}</div>
                    <span
                        className={`opacity-50 overflow-hidden transition-width nowrap ${
                            tab === dashboard.Tab.ide ? 'm-2 w-16' : 'w-0'
                        }`}
                    >
                        {projectName ?? 'No project open'}
                    </span>
                </div>
                <div className="flex items-center bg-label rounded-full px-3 w-[33rem] max-w-2xl">
                    <div className="mr-2">{ICONS.search}</div>
                    <input
                        className="flex-1 bg-transparent"
                        type="text"
                        placeholder="Click here or start typing to search for projects, data connectors, users, and more ..."
                        value={searchVal}
                        onChange={event => {
                            setSearchVal(event.target.value)
                        }}
                    />
                </div>
            </div>
            <div className="flex items-stretch">
                <a
                    className="flex items-center bg-[#3F68CE] rounded-full px-2.5 text-white mr-4"
                    href="https://discord.gg/enso"
                >
                    <span>help chat</span>
                    <div className="ml-2">{ICONS.chat}</div>
                </a>
                {/* User profile and menu. */}
                <div className="transform">
                    <div
                        style={{
                            backgroundImage: `url(https://faces-img.xcdn.link/image-lorem-face-4742.jpg)`,
                        }}
                        className="rounded-full w-8 h-8 bg-cover cursor-pointer"
                        onClick={() => {
                            setIsOpenMenu(v => !v)
                        }}
                    />
                    {isOpenMenu ? <UserMenu /> : null}
                </div>
            </div>
        </div>
    )
}

export default TopBar
