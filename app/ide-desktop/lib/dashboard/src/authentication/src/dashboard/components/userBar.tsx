/** @file A toolbar containing chat and the user menu. */
import * as React from 'react'

import ChatIcon from 'enso-assets/chat.svg'
import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as modalProvider from '../../providers/modal'

import Button from './button'
import UserMenu from './userMenu'

// ===============
// === UserBar ===
// ===============

/** Props for a {@link UserBar}. */
export interface UserBarProps {
    isHelpChatOpen: boolean
    setIsHelpChatOpen: (isHelpChatOpen: boolean) => void
    onSignOut: () => void
}

/** A toolbar containing chat and the user menu. */
export default function UserBar(props: UserBarProps) {
    const { isHelpChatOpen, setIsHelpChatOpen, onSignOut } = props
    const { updateModal } = modalProvider.useSetModal()
    return (
        <div className="flex shrink-0 items-center bg-frame-bg rounded-full gap-3 h-8 pl-2 pr-0.75 cursor-default pointer-events-auto">
            <Button
                active={isHelpChatOpen}
                image={ChatIcon}
                onClick={() => {
                    setIsHelpChatOpen(!isHelpChatOpen)
                }}
            />
            <button
                onClick={event => {
                    event.stopPropagation()
                    updateModal(oldModal =>
                        oldModal?.type === UserMenu ? null : <UserMenu onSignOut={onSignOut} />
                    )
                }}
            >
                <img src={DefaultUserIcon} height={28} width={28} />
            </button>
        </div>
    )
}
