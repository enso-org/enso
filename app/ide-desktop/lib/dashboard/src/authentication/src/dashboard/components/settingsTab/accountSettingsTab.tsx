/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as authProvider from '../../../authentication/providers/auth'
import * as backendProvider from '../../../providers/backend'
import * as hooks from '../../../hooks'
import * as modalProvider from '../../../providers/modal'

import ConfirmDeleteUserModal from '../confirmDeleteUserModal'

// =================
// === InfoEntry ===
// =================

/** Props for a transparent wrapper component. */
interface InternalTransparentWrapperProps {
    children: React.ReactNode
}

/** A transparent wrapper component */
// This is a React component even though it does not contain JSX.
// eslint-disable-next-line no-restricted-syntax
function Name(props: InternalTransparentWrapperProps) {
    return props.children
}

/** A transparent wrapper component */
// This is a React component even though it does not contain JSX.
// eslint-disable-next-line no-restricted-syntax
function Value(props: InternalTransparentWrapperProps) {
    return props.children
}

/** Props for a {@link InfoEntry}. */
interface InternalInfoEntryProps {
    children: [React.ReactNode, React.ReactNode]
}

/** Styled information display containing key and value. */
function InfoEntry(props: InternalInfoEntryProps) {
    const { children } = props
    const [name, value] = children
    return (
        <div className="flex gap-4.75">
            <span className="leading-5 w-12 h-8 py-1.25">{name}</span>
            <span className="font-bold leading-5 h-8 py-1.25">{value}</span>
        </div>
    )
}

// ==========================
// === AccountSettingsTab ===
// ==========================

/** Settings tab for viewing and editing account information. */
export default function AccountSettingsTab() {
    const toastAndLog = hooks.useToastAndLog()
    const { setModal } = modalProvider.useSetModal()
    const { backend } = backendProvider.useBackend()
    const { organization } = authProvider.useNonPartialUserSession()

    const doSubmit = async (event: React.ChangeEvent<HTMLInputElement>) => {
        const image = event.target.files?.[0]
        if (image == null) {
            toastAndLog('Could not upload a new profile picture because no image was found')
        } else {
            try {
                await backend.uploadUserPicture(image)
            } catch (error) {
                toastAndLog(null, error)
            }
        }
        // Reset selected files.
        event.target.value = ''
    }

    return (
        <div className="flex gap-8">
            <div className="flex flex-col gap-8">
                <div className="flex flex-col gap-2.5">
                    <h3 className="font-bold text-xl h-9.5 py-0.5">User Account</h3>
                    <div className="flex flex-col">
                        <InfoEntry>
                            <Name>Name</Name>
                            <Value>{organization?.name ?? ''}</Value>
                        </InfoEntry>
                        <InfoEntry>
                            <Name>Email</Name>
                            <Value>{organization?.email ?? ''}</Value>
                        </InfoEntry>
                    </div>
                </div>
                <div className="flex flex-col gap-2.5 rounded-2.5xl border-2 border-danger px-4 pt-2.25 pb-3.75">
                    <h3 className="text-danger font-bold text-xl h-9.5 py-0.5">Danger Zone</h3>
                    <div className="flex gap-2">
                        <button
                            className="rounded-full bg-danger text-inversed px-2 py-1"
                            onClick={event => {
                                event.stopPropagation()
                                setModal(<ConfirmDeleteUserModal />)
                            }}
                        >
                            <span className="leading-5 h-6 py-px">Delete this organization</span>
                        </button>
                        <span className="leading-5 h-8 py-1.25">
                            Once deleted, it will be gone forever. Please be certain.
                        </span>
                    </div>
                </div>
            </div>
            <div className="flex flex-col gap-2.5">
                <h3 className="font-bold text-xl h-9.5 py-0.5">Profile picture</h3>
                <label className="cursor-pointer rounded-full h-32 w-32 hover:bg-frame transition-colors">
                    <input type="file" className="hidden" accept="image/*" onChange={doSubmit} />
                    <img
                        src={organization?.profilePicture ?? DefaultUserIcon}
                        className="w-full h-full"
                    />
                </label>
                <span className="py-1 w-64">
                    Your organization&apos;s profile picture should not be irrelevant, abusive or
                    vulgar. It should not be a default image provided by Enso.
                </span>
            </div>
        </div>
    )
}
