/** @file The top-bar of dashboard. */
import { FC, useState } from 'react'

import * as hooks from '../../../hooks'
import UserMenu from './userMenu'

// ==============
// === TopBar ===
// ==============

/** ICONS constant using by TopBar. */
const ICONS = {
    BAR: (
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
    SEARCH: (
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
    CHAT: (
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

/** The hardcoded avatar. */
const testAvatarUrl =
    'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAH8AAAB/CAMAAADxY+0hAAAAWlBMVEX///9UWV1MUVaSlZc/RUrZ2tpRVlpJT1NGTFBDSU75+fnk5eXP0NFfZGducnWtr7CJjI40O0GAg4abnZ/x8fK7vb6ho6Xq6+vExcZkaWx2en2nqaq1t7g5QEWb+0SXAAADTklEQVRoge2a2ZKrIBBABREa97jEmOj//+bVyZjcmXKhmca7FOchVXk60ECDDUHg8XjwNEk6XMMwvA5p0pwt74oq0wIU51yB0FlVdCfa23xUUrI3Uqoxb8+RxwXA7+5XGwCK2L3+lqkV+ROV3Rzbm1Ks9f0VA1E6nYoR2+78ZwhY5E7f7nZ+CYGzeTjoQ/uMHtzoWzP91AAnEUjBUM8YpPT6jh+P/YLk9NmwN9dPDeip9Y+jhfcV9aDVdwKlZ0zQjkDJkX5eUuoTbPenACSEfnT3aQPQoe0zdDPAOPP9DmEWzDFrf0HmVPrORj81gGoAUlzuWVBUu0Bh6S+I/BV+9c3wisifWY5/RuTHJ78ngsh/sfRfvP+/8Ntk/xlN5Ecd/d6QHQItdv8ZshPAn86/qWX/qfafxkrPGNmn+N3q/HGn0gc3q/MXXS2ksdgBZUZYCanxK0DVdPqgM//2XgDSDzB0ADhl94MgxgYAiEuBLa4BQF6CuWOSIKdb+wsNQ9Rf6FLfm8S4ACQ55bf3C+MCmIvy10cDtEkEpHakD4JotfD+TQ8OC8BNfrQT6dxpATx+7IZAwsP1FUSUq60WSJU7jP2LW6/XchHXvevbj4U0BPElG0guoHQ27VeI0/rOhNAAoIVg9zo94erpWxOaJL0Nw3BLk+Z0ucfzFxDHTXz23E/StgirXo6Xmc9f2Vdh0aZOzh0vmqit+ynXgeJSft0Epv9cwZQT+7qNXOyAcTJUvdKHZzDJteqrIaEdleSaC5DG5z8JIr+SjUVcZCPi8nGJw5iRPEeIQtjc8A+aoCD86Xkgqbhd8eUJ59VPhqEJV08aqBbo0Ho5DOqn9o8WKLv3AF2G/+hfBzKLWsANPeW3kRx9MqypOv8EkOWIilY/NQB1GZTbVVz3UIjryIpePzXAOAIldfCfgGE9/OFGPzXA6FFGNDrSMzYabAcx4br/juTHO6JFqdWc46Js4mrwn8DRdmh512zK0Z105DL6M2p/CobO/eGePnY39xfk3hKwfGmBYfdVRu129s3sXgxYvnTAsPsqwvamG8POrXjjLvW/GbfPwxbP3PDsPIyLTvFvZyDv937v937v937v9/7T/Rfunsu2PylD95RuL0k8nn+IX7ffNgoiPWOuAAAAAElFTkSuQmCC'

interface TopBarProps {
    bindSearchVal: ReturnType<typeof hooks.useInput>['bind']
}

/**
 * The {@link TopBarProps["bindSearchVal"]} params is used to communicate with the parent component.
 * Because searchVal maybe change parent component's project list.
 */
const TopBar: FC<TopBarProps> = props => {
    const { bindSearchVal } = props

    const [isOpenMenu, setIsOpenMenu] = useState(false)

    return (
        <div className="flex justify-between m-2 text-[11.5px] h-8">
            <div className="flex ml-[80px] text-[#3E515F]">
                <div className="flex items-center bg-[#3E515F] bg-opacity-5 rounded-full pl-1 pr-2.5 mr-4 cursor-pointer">
                    <div
                        className="bg-white px-1.5 py-1 rounded-full mr-2"
                        style={{ boxShadow: BAR_BOX_SHADOW }}
                    >
                        {ICONS.BAR}
                    </div>
                    <span className="opacity-50">My current project</span>
                </div>
                <div className="flex items-center bg-[#3E515F] bg-opacity-5 rounded-full px-3 w-[33rem] max-w-2xl">
                    <div className="mr-2">{ICONS.SEARCH}</div>
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
                    <div className="ml-2">{ICONS.CHAT}</div>
                </a>
                {/* User profile and menu. */}
                <div className="transform">
                    <div
                        style={{ backgroundImage: `url(${testAvatarUrl})` }}
                        className="rounded-full w-8 h-8 bg-cover cursor-pointer"
                        onClick={() => setIsOpenMenu(v => !v)}
                    />
                    {isOpenMenu ? <UserMenu /> : null}
                </div>
            </div>
        </div>
    )
}

export default TopBar
