/** @file A small toolbar containing chat and the user menu. */
import * as React from 'react'

import ChatIcon from 'enso-assets/chat.svg'
import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as modalProvider from '../../providers/modal'

import UserMenu from './userMenu'

/** Props for a {@link UserBar}. */
export interface UserBarProps {
    isHelpChatOpen: boolean
    setIsHelpChatOpen: (isHelpChatOpen: boolean) => void
}

/** A small toolbar containing chat and the user menu. */
export default function UserBar(props: UserBarProps) {
    const { isHelpChatOpen, setIsHelpChatOpen } = props
    const { updateModal } = modalProvider.useSetModal()
    return (
        <div className="flex items-center bg-frame-bg rounded-full gap-3 h-8 pl-2 pr-0.75">
            <button
                className={isHelpChatOpen ? '' : 'opacity-50'}
                onClick={() => {
                    setIsHelpChatOpen(!isHelpChatOpen)
                }}
            >
                <img src={ChatIcon} />
            </button>
            <button
                onClick={event => {
                    event.stopPropagation()
                    updateModal(oldModal => (oldModal?.type === UserMenu ? null : <UserMenu />))
                }}
            >
                <img src={DefaultUserIcon} height={28} width={28} />
            </button>
        </div>
    )
}
