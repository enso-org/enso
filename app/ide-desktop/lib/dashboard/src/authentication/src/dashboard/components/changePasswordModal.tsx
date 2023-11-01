/** @file A modal for changing the user's password. */
import * as React from 'react'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import LockIcon from 'enso-assets/lock.svg'

import * as auth from '../../authentication/providers/auth'
import * as modalProvider from '../../providers/modal'
import * as string from '../../string'
import * as validation from '../validation'

import Input from './input'
import Modal from './modal'
import SubmitButton from '../../authentication/components/submitButton'
import SvgIcon from '../../authentication/components/svgIcon'

// ===========================
// === ChangePasswordModal ===
// ===========================

/** A modal for changing the user's password. */
export default function ChangePasswordModal() {
    const { changePassword } = auth.useAuth()
    const { unsetModal } = modalProvider.useSetModal()

    const [oldPassword, setOldPassword] = React.useState('')
    const [newPassword, setNewPassword] = React.useState('')
    const [confirmNewPassword, setConfirmNewPassword] = React.useState('')
    const [isSubmitting, setIsSubmitting] = React.useState(false)

    return (
        <Modal centered className="bg-dim">
            <form
                data-testid="change-password-modal"
                className="flex flex-col gap-6 bg-frame-selected backdrop-blur-3xl rounded-2xl p-8 w-full max-w-md"
                onSubmit={async event => {
                    event.preventDefault()
                    setIsSubmitting(true)
                    const success = await changePassword(oldPassword, newPassword)
                    setIsSubmitting(false)
                    if (success) {
                        unsetModal()
                    }
                }}
                onClick={event => {
                    event.stopPropagation()
                }}
            >
                <div className="self-center text-xl">Change your password</div>
                <label className="flex flex-col gap-1">
                    Old password
                    <div className="relative">
                        <SvgIcon src={LockIcon} />
                        <Input
                            autoFocus
                            required
                            validate
                            id="old_password"
                            type="password"
                            name="old_password"
                            autoComplete="current-password"
                            placeholder="Enter your old password"
                            pattern={validation.PASSWORD_PATTERN}
                            error={validation.PASSWORD_ERROR}
                            value={oldPassword}
                            setValue={setOldPassword}
                            className="text-sm placeholder-gray-500 pl-10 pr-4 rounded-full w-full py-2"
                        />
                    </div>
                </label>
                <label className="flex flex-col gap-1">
                    New password
                    <div className="relative">
                        <SvgIcon src={LockIcon} />
                        <Input
                            required
                            validate
                            type="password"
                            autoComplete="new-password"
                            placeholder="Enter your new password"
                            pattern={validation.PASSWORD_PATTERN}
                            error={validation.PASSWORD_ERROR}
                            value={newPassword}
                            setValue={setNewPassword}
                            className="text-sm placeholder-gray-500 pl-10 pr-4 rounded-full w-full py-2"
                        />
                    </div>
                </label>
                <label className="flex flex-col gap-1">
                    Confirm new password
                    <div className="relative">
                        <SvgIcon src={LockIcon} />
                        <Input
                            required
                            validate
                            type="password"
                            autoComplete="new-password"
                            placeholder="Confirm your new password"
                            pattern={string.regexEscape(newPassword)}
                            error={validation.CONFIRM_PASSWORD_ERROR}
                            value={confirmNewPassword}
                            setValue={setConfirmNewPassword}
                            className="text-sm placeholder-gray-500 pl-10 pr-4 rounded-full w-full py-2"
                        />
                    </div>
                </label>
                <SubmitButton disabled={isSubmitting} text="Reset" icon={ArrowRightIcon} />
            </form>
        </Modal>
    )
}
