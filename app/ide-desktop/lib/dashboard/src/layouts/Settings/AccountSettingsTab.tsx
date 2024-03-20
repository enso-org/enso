/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import DefaultUserIcon from 'enso-assets/default_user.svg'
import EyeCrossedIcon from 'enso-assets/eye_crossed.svg'
import EyeIcon from 'enso-assets/eye.svg'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import SvgMask from '#/components/SvgMask'

import ConfirmDeleteUserModal from '#/modals/ConfirmDeleteUserModal'

import * as object from '#/utilities/object'
import * as uniqueString from '#/utilities/uniqueString'
import * as validation from '#/utilities/validation'

// =============
// === Input ===
// =============

/** Props for an {@link Input}. */
interface InternalInputProps {
  readonly originalValue: string
  readonly type?: string
  readonly placeholder?: string
  readonly onChange?: React.ChangeEventHandler<HTMLInputElement>
  readonly onSubmit?: (value: string) => void
}

/** A styled input. */
function Input(props: InternalInputProps) {
  const { originalValue, type, placeholder, onChange, onSubmit } = props
  const [isShowingPassword, setIsShowingPassword] = React.useState(false)
  const cancelled = React.useRef(false)

  const onKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
    switch (event.key) {
      case 'Escape': {
        cancelled.current = true
        event.stopPropagation()
        event.currentTarget.value = originalValue
        event.currentTarget.blur()
        break
      }
      case 'Enter': {
        cancelled.current = false
        event.stopPropagation()
        event.currentTarget.blur()
        break
      }
      case 'Tab': {
        cancelled.current = false
        event.currentTarget.blur()
        break
      }
      default: {
        cancelled.current = false
        break
      }
    }
  }

  const input = (
    <input
      className="settings-value w-full rounded-full bg-transparent font-bold placeholder-black/30 transition-colors invalid:border invalid:border-red-700 hover:bg-selected-frame focus:bg-selected-frame"
      type={isShowingPassword ? 'text' : type}
      size={1}
      defaultValue={originalValue}
      placeholder={placeholder}
      onKeyDown={onKeyDown}
      onChange={onChange}
      onBlur={event => {
        if (!cancelled.current) {
          onSubmit?.(event.currentTarget.value)
        }
      }}
    />
  )

  return type !== 'password' ? (
    input
  ) : (
    <div className="relative">
      {input}
      {
        <SvgMask
          src={isShowingPassword ? EyeIcon : EyeCrossedIcon}
          className="absolute right-2 top-1 cursor-pointer rounded-full"
          onClick={() => {
            setIsShowingPassword(show => !show)
          }}
        />
      }
    </div>
  )
}

// ==========================
// === AccountSettingsTab ===
// ==========================

/** Settings tab for viewing and editing account information. */
export default function AccountSettingsTab() {
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { setUser, changePassword, signOut } = authProvider.useAuth()
  const { setModal } = modalProvider.useSetModal()
  const { backend } = backendProvider.useBackend()
  const { user, accessToken } = authProvider.useNonPartialUserSession()
  const { getText } = textProvider.useText()
  const [passwordFormKey, setPasswordFormKey] = React.useState('')
  const [currentPassword, setCurrentPassword] = React.useState('')
  const [newPassword, setNewPassword] = React.useState('')
  const [confirmNewPassword, setConfirmNewPassword] = React.useState('')

  // The shape of the JWT payload is statically known.
  // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
  const username: string | null =
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-non-null-assertion
    accessToken != null ? JSON.parse(atob(accessToken.split('.')[1]!)).username : null
  const canChangePassword = username != null ? !/^Github_|^Google_/.test(username) : false
  const canSubmitPassword =
    currentPassword !== '' &&
    newPassword !== '' &&
    confirmNewPassword !== '' &&
    newPassword === confirmNewPassword &&
    validation.PASSWORD_REGEX.test(newPassword)

  const doUpdateName = async (newName: string) => {
    const oldName = user?.name ?? ''
    if (newName === oldName) {
      return
    } else {
      try {
        await backend.updateUser({ username: newName })
        setUser(object.merger({ name: newName }))
      } catch (error) {
        toastAndLog(null, error)
      }
      return
    }
  }

  const doUploadUserPicture = async (event: React.ChangeEvent<HTMLInputElement>) => {
    const image = event.target.files?.[0]
    if (image == null) {
      toastAndLog('noNewProfilePictureError')
    } else {
      try {
        const newUser = await backend.uploadUserPicture({ fileName: image.name }, image)
        setUser(newUser)
      } catch (error) {
        toastAndLog(null, error)
      }
    }
    // Reset selected files, otherwise the file input will do nothing if the same file is
    // selected again. While technically not undesired behavior, it is unintuitive for the user.
    event.target.value = ''
  }

  return (
    <div className="flex h flex-col gap-settings-section lg:h-auto lg:flex-row">
      <div className="flex w-settings-main-section flex-col gap-settings-subsection">
        <div className="flex flex-col gap-settings-section-header">
          <h3 className="settings-subheading">{getText('userAccount')}</h3>
          <div className="flex flex-col">
            <div className="flex h-row gap-settings-entry">
              <span className="text my-auto w-user-account-settings-label">{getText('name')}</span>
              <span className="text my-auto grow font-bold">
                <Input originalValue={user?.name ?? ''} onSubmit={doUpdateName} />
              </span>
            </div>
            <div className="flex h-row gap-settings-entry">
              <span className="text my-auto w-user-account-settings-label">{getText('email')}</span>
              <span className="settings-value my-auto grow font-bold">{user?.email ?? ''}</span>
            </div>
          </div>
        </div>
        {canChangePassword && (
          <div key={passwordFormKey}>
            <h3 className="settings-subheading">{getText('changePassword')}</h3>
            <div className="flex h-row gap-settings-entry">
              <span className="text my-auto w-change-password-settings-label">
                {getText('currentPasswordLabel')}
              </span>
              <span className="text my-auto grow font-bold">
                <Input
                  type="password"
                  originalValue=""
                  placeholder={getText('currentPasswordPlaceholder')}
                  onChange={event => {
                    setCurrentPassword(event.currentTarget.value)
                  }}
                />
              </span>
            </div>
            <div className="flex h-row gap-settings-entry">
              <span className="text my-auto w-change-password-settings-label">
                {getText('newPasswordLabel')}
              </span>
              <span className="text my-auto grow font-bold">
                <Input
                  type="password"
                  originalValue=""
                  placeholder={getText('newPasswordPlaceholder')}
                  onChange={event => {
                    const newValue = event.currentTarget.value
                    setNewPassword(newValue)
                    event.currentTarget.setCustomValidity(
                      newValue === '' || validation.PASSWORD_REGEX.test(newValue)
                        ? ''
                        : getText('passwordValidationError')
                    )
                  }}
                />
              </span>
            </div>
            <div className="flex h-row gap-settings-entry">
              <span className="text my-auto w-change-password-settings-label">
                {getText('confirmNewPasswordLabel')}
              </span>
              <span className="text my-auto grow font-bold">
                <Input
                  type="password"
                  originalValue=""
                  placeholder={getText('confirmNewPasswordPlaceholder')}
                  onChange={event => {
                    const newValue = event.currentTarget.value
                    setConfirmNewPassword(newValue)
                    event.currentTarget.setCustomValidity(
                      newValue === '' || newValue === newPassword ? '' : 'Passwords must match.'
                    )
                  }}
                />
              </span>
            </div>
            <div className="flex h-row items-center gap-buttons">
              <button
                type="submit"
                disabled={!canSubmitPassword}
                className={`settings-value rounded-full bg-invite font-medium text-white selectable enabled:active`}
                onClick={() => {
                  setPasswordFormKey(uniqueString.uniqueString())
                  setCurrentPassword('')
                  setNewPassword('')
                  setConfirmNewPassword('')
                  void changePassword(currentPassword, newPassword)
                }}
              >
                {getText('change')}
              </button>
              <button
                type="button"
                disabled={!canSubmitPassword}
                className="settings-value rounded-full bg-selected-frame font-medium selectable enabled:active"
                onClick={() => {
                  setPasswordFormKey(uniqueString.uniqueString())
                  setCurrentPassword('')
                  setNewPassword('')
                  setConfirmNewPassword('')
                }}
              >
                {getText('cancel')}
              </button>
            </div>
          </div>
        )}
        {/* This UI element does not appear anywhere else. */}
        {/* eslint-disable-next-line no-restricted-syntax */}
        <div className="flex flex-col items-start gap-settings-section-header rounded-2.5xl border-2 border-danger px-[1rem] pb-[0.9375rem] pt-[0.5625rem]">
          <h3 className="settings-subheading text-danger">{getText('dangerZone')}</h3>
          <div className="flex gap-buttons">
            <button
              className="button bg-danger px-delete-user-account-button-x text-inversed opacity-full hover:opacity-full"
              onClick={event => {
                event.stopPropagation()
                setModal(
                  <ConfirmDeleteUserModal
                    doDelete={async () => {
                      await backend.deleteUser()
                      await signOut()
                    }}
                  />
                )
              }}
            >
              <span className="text inline-block">{getText('deleteUserAccountButtonLabel')}</span>
            </button>
            <span className="text my-auto">{getText('deleteUserAccountWarning')}</span>
          </div>
        </div>
      </div>
      <div className="flex flex-col gap-settings-section-header">
        <h3 className="settings-subheading">{getText('profilePicture')}</h3>
        <label className="flex h-profile-picture-large w-profile-picture-large cursor-pointer items-center overflow-clip rounded-full transition-colors hover:bg-frame">
          <input type="file" className="hidden" accept="image/*" onChange={doUploadUserPicture} />
          <img
            src={user?.profilePicture ?? DefaultUserIcon}
            width={128}
            height={128}
            className="pointer-events-none"
          />
        </label>
        <span className="w-profile-picture-caption py-profile-picture-caption-y">
          {getText('profilePictureWarning')}
        </span>
      </div>
    </div>
  )
}
