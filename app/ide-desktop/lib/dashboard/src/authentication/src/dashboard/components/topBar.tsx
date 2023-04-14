/** @file The top-bar of dashboard. */
import * as modalProvider from '../../providers/modal'
import * as svg from '../../components/svg'

import UserMenu from './userMenu'

// ==============
// === TopBar ===
// ==============

interface TopBarProps {
    searchVal: string
    setSearchVal: (value: string) => void
}

/**
 * The {@link TopBarProps.setSearchVal} param is used to communicate with the parent component,
 * because `searchVal` may change parent component's project list.
 */
function TopBar(props: TopBarProps) {
    const { searchVal, setSearchVal } = props
    const { setModal } = modalProvider.useSetModal()

    return (
        <div className="flex m-2 h-8">
            <div className="flex text-primary">
                <div className="flex items-center bg-label rounded-full px-1 mx-2 cursor-pointer">
                    <div className="bg-white shadow-soft rounded-full px-1.5 py-1">
                        {svg.BARS_ICON}
                    </div>
                    <span className="opacity-50 mx-2">My current project</span>
                </div>
                <div className="flex items-center bg-label rounded-full px-2 w-140 max-w-2xl">
                    <div className="mr-2">{svg.MAGNIFYING_GLASS_ICON}</div>
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
            <div className="grow" />
            <a
                href="https://discord.gg/enso"
                className="flex items-center bg-help rounded-full px-2.5 text-white mx-2"
            >
                <span>help chat</span>
                <div className="ml-2">{svg.SPEECH_BUBBLE_ICON}</div>
            </a>
            {/* User profile and menu. */}
            <div className="transform">
                <img
                    src="https://faces-img.xcdn.link/image-lorem-face-4742.jpg"
                    className="rounded-full w-8 h-8 bg-cover cursor-pointer"
                    onClick={event => {
                        event.stopPropagation()
                        setModal(() => <UserMenu />)
                    }}
                />
            </div>
        </div>
    )
}

export default TopBar
