/** @file The top-bar of dashboard. */
import * as react from 'react'

import * as hooks from '../../hooks'
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
/** The BAR icon shadow. */
const BAR_BOX_SHADOW =
    '0px 18px 80px rgba(0, 0, 0, 0.11), 0px 7.51997px 33.4221px rgba(0, 0, 0, 0.079074), 0px 4.02054px 17.869px rgba(0, 0, 0, 0.0655718), 0px 2.25388px 10.0172px rgba(0, 0, 0, 0.055), 0px 1.19702px 5.32008px rgba(0, 0, 0, 0.0444282), 0px 0.498106px 2.21381px rgba(0, 0, 0, 0.030926)'

interface TopBarProps {
    // `1` is special-cased in the eslint configuration, but there is a bug in this rule.
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    bindSearchVal: ReturnType<typeof hooks.useInput>[1]
}

/**
 * The {@link TopBarProps["bindSearchVal"]} params is used to communicate with the parent component.
 * Because searchVal maybe change parent component's project list.
 */
function TopBar(props: TopBarProps) {
    const { bindSearchVal } = props

    const [isOpenMenu, setIsOpenMenu] = react.useState(false)

    return (
        <div className="flex justify-between m-2 text-[11.5px] h-8">
            <div className="flex ml-[80px] text-[#3E515F]">
                <div
                    className={`flex items-center bg-[#3E515F] bg-opacity-5 rounded-full pl-1 
                                pr-2.5 mr-4 cursor-pointer`}
                >
                    <div
                        className="bg-white px-1.5 py-1 rounded-full mr-2"
                        style={{ boxShadow: BAR_BOX_SHADOW }}
                    >
                        {ICONS.bar}
                    </div>
                    <span className="opacity-50">My current project</span>
                </div>
                <div
                    className={`flex items-center bg-[#3E515F] bg-opacity-5 rounded-full px-3 
                                w-[33rem] max-w-2xl`}
                >
                    <div className="mr-2">{ICONS.search}</div>
                    <input
                        {...bindSearchVal}
                        className="flex-1 bg-transparent"
                        type="text"
                        placeholder="Click here or start typing to search for projects, data connectors, users, and more ..."
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
