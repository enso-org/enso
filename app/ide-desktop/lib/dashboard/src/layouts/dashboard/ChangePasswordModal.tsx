/** @file A modal for changing the user's password. */
import * as React from 'react'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import LockIcon from 'enso-assets/lock.svg'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'
import * as string from '#/utilities/string'
import * as validation from '#/utilities/validation'

import Input from '#/components/Input'
import Modal from '#/components/Modal'
import SubmitButton from '#/components/SubmitButton'

// ===========================
// === ChangePasswordModal ===
// ===========================

/** A modal for changing the user's password. */
export default function ChangePasswordModal() {
  const { changePassword } = authProvider.useAuth()
  const { unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
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
        <div className="self-center text-xl">{getText('changeYourPassword')}</div>
        <Input
          autoFocus
          required
          validate
          allowShowingPassword
          id="old_password"
          type="password"
          name="old_password"
          autoComplete="current-password"
          label={getText('oldPasswordLabel')}
          icon={LockIcon}
          placeholder={getText('oldPasswordPlaceholder')}
          error={getText('passwordValidationError')}
          value={oldPassword}
          setValue={setOldPassword}
          className="text-sm placeholder-gray-500 pl-10 pr-4 rounded-full w-full py-2"
        />
        <Input
          required
          validate
          allowShowingPassword
          type="password"
          autoComplete="new-password"
          label={getText('newPasswordLabel')}
          icon={LockIcon}
          placeholder={getText('newPasswordPlaceholder')}
          pattern={validation.PASSWORD_PATTERN}
          error={getText('passwordValidationError')}
          value={newPassword}
          setValue={setNewPassword}
          className="text-sm placeholder-gray-500 pl-10 pr-4 rounded-full w-full py-2"
        />
        <Input
          required
          validate
          allowShowingPassword
          type="password"
          autoComplete="new-password"
          label={getText('confirmNewPasswordLabel')}
          icon={LockIcon}
          placeholder={getText('confirmNewPasswordPlaceholder')}
          pattern={string.regexEscape(newPassword)}
          error={getText('passwordMismatchError')}
          value={confirmNewPassword}
          setValue={setConfirmNewPassword}
          className="text-sm placeholder-gray-500 pl-10 pr-4 rounded-full w-full py-2"
        />
        <SubmitButton disabled={isSubmitting} text={getText('reset')} icon={ArrowRightIcon} />
      </form>
    </Modal>
  )
}
