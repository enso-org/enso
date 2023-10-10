/** @file A toolbar containing chat and the user menu. */
import * as React from 'react'

import ChatIcon from 'enso-assets/chat.svg'
import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as authProvider from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as modalProvider from '../../providers/modal'

import * as pageSwitcher from './pageSwitcher'
import Button from './button'
import ManagePermissionsModal from './managePermissionsModal'
import UserMenu from './userMenu'

// ===============
// === UserBar ===
// ===============

/** Props for a {@link UserBar}. */
export interface UserBarProps {
    page: pageSwitcher.Page
    isHelpChatOpen: boolean
    setIsHelpChatOpen: (isHelpChatOpen: boolean) => void
    projectAsset: backendModule.ProjectAsset | null
    setProjectAsset: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>> | null
    doRemoveSelf: () => void
    onSignOut: () => void
}

/** A toolbar containing chat and the user menu. */
export default function UserBar(props: UserBarProps) {
    const {
        page,
        isHelpChatOpen,
        setIsHelpChatOpen,
        projectAsset,
        setProjectAsset,
        doRemoveSelf,
        onSignOut,
    } = props
    const { organization } = authProvider.useNonPartialUserSession()
    const { updateModal } = modalProvider.useSetModal()
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()
    const self =
        organization != null
            ? projectAsset?.permissions?.find(
                  permissions => permissions.user.user_email === organization.email
              ) ?? null
            : null
    const shouldShowShareButton =
        backend.type === backendModule.BackendType.remote &&
        page === pageSwitcher.Page.editor &&
        projectAsset != null &&
        setProjectAsset != null &&
        self != null
    return (
        <div className="flex shrink-0 items-center bg-frame backdrop-blur-3xl rounded-full gap-3 h-8 pl-2 pr-0.75 cursor-default pointer-events-auto">
            <Button
                active={isHelpChatOpen}
                image={ChatIcon}
                onClick={() => {
                    setIsHelpChatOpen(!isHelpChatOpen)
                }}
            />
            {shouldShowShareButton && (
                <button
                    className="text-inversed bg-share rounded-full leading-5 h-6 px-2 py-px"
                    onClick={event => {
                        event.stopPropagation()
                        setModal(
                            <ManagePermissionsModal
                                item={projectAsset}
                                setItem={setProjectAsset}
                                self={self}
                                doRemoveSelf={doRemoveSelf}
                                eventTarget={null}
                            />
                        )
                    }}
                >
                    Share
                </button>
            )}
            <button
                onClick={event => {
                    event.stopPropagation()
                    updateModal(oldModal =>
                        oldModal?.type === UserMenu ? null : <UserMenu onSignOut={onSignOut} />
                    )
                }}
            >
                <img
                    src={DefaultUserIcon}
                    height={28}
                    width={28}
                    onDragStart={event => {
                        event.preventDefault()
                    }}
                />
            </button>
        </div>
    )
}
