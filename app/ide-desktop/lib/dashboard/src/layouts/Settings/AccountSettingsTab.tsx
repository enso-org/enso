/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import DefaultUserIcon from 'enso-assets/default_user.svg'
import EyeCrossedIcon from 'enso-assets/eye_crossed.svg'
import EyeIcon from 'enso-assets/eye.svg'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'

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
      className="settings-value rounded-full font-bold w-full bg-transparent hover:bg-selected-frame focus:bg-selected-frame transition-colors placeholder-black/30 invalid:border invalid:border-red-700"
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
          className="absolute cursor-pointer rounded-full right-2 top-1"
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
      toastAndLog('Could not upload a new profile picture because no image was found')
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
    <div className="flex flex-col lg:flex-row gap-settings-section h lg:h-auto">
      <div className="flex flex-col gap-settings-subsection w-settings-main-section">
        <div className="flex flex-col gap-settings-section-header">
          <h3 className="settings-subheading">User Account</h3>
          <div className="flex flex-col">
            <div className="flex h-row gap-settings-entry">
              <span className="w-user-account-settings-label text my-auto">Name</span>
              <span className="grow font-bold text my-auto">
                <Input originalValue={user?.name ?? ''} onSubmit={doUpdateName} />
              </span>
            </div>
            <div className="flex h-row gap-settings-entry">
              <span className="w-user-account-settings-label text my-auto">Email</span>
              <span className="settings-value grow font-bold my-auto">{user?.email ?? ''}</span>
            </div>
          </div>
        </div>
        {canChangePassword && (
          <div key={passwordFormKey}>
            <h3 className="settings-subheading">Change Password</h3>
            <div className="flex h-row gap-settings-entry">
              <span className="w-change-password-settings-label text my-auto">
                Current Password
              </span>
              <span className="grow font-bold text my-auto">
                <Input
                  type="password"
                  originalValue=""
                  placeholder="Enter your current password"
                  onChange={event => {
                    setCurrentPassword(event.currentTarget.value)
                  }}
                />
              </span>
            </div>
            <div className="flex h-row gap-settings-entry">
              <span className="w-change-password-settings-label text my-auto">New Password</span>
              <span className="grow font-bold text my-auto">
                <Input
                  type="password"
                  originalValue=""
                  placeholder="Enter your new password"
                  onChange={event => {
                    const newValue = event.currentTarget.value
                    setNewPassword(newValue)
                    event.currentTarget.setCustomValidity(
                      newValue === '' || validation.PASSWORD_REGEX.test(newValue)
                        ? ''
                        : validation.PASSWORD_ERROR
                    )
                  }}
                />
              </span>
            </div>
            <div className="flex h-row gap-settings-entry">
              <span className="w-change-password-settings-label text my-auto">
                Confirm New Password
              </span>
              <span className="grow font-bold text my-auto">
                <Input
                  type="password"
                  originalValue=""
                  placeholder="Confirm your new password"
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
            <div className="flex items-center h-row gap-buttons">
              <button
                disabled={
                  currentPassword === '' ||
                  newPassword === '' ||
                  confirmNewPassword === '' ||
                  newPassword !== confirmNewPassword ||
                  !validation.PASSWORD_REGEX.test(newPassword)
                }
                type="submit"
                className="settings-value text-white bg-invite font-medium rounded-full disabled:opacity-disabled"
                onClick={() => {
                  setPasswordFormKey(uniqueString.uniqueString())
                  setCurrentPassword('')
                  setNewPassword('')
                  setConfirmNewPassword('')
                  void changePassword(currentPassword, newPassword)
                }}
              >
                Change
              </button>
              <button
                type="button"
                className="settings-value bg-selected-frame font-medium rounded-full"
                onClick={() => {
                  setPasswordFormKey(uniqueString.uniqueString())
                  setCurrentPassword('')
                  setNewPassword('')
                  setConfirmNewPassword('')
                }}
              >
                Cancel
              </button>
            </div>
          </div>
        )}
        {/* This UI element does not appear anywhere else. */}
        {/* eslint-disable-next-line no-restricted-syntax */}
        <div className="flex flex-col items-center gap-settings-section-header rounded-2.5xl border-2 border-danger px-[16px] pt-[9px] pb-[15px]">
          <h3 className="settings-subheading text-danger">Danger Zone</h3>
          <div className="flex gap-buttons">
            <button
              className="button bg-danger text-inversed px-delete-user-account-button-x"
              onClick={event => {
                event.stopPropagation()
                setModal(
                  <ConfirmDeleteUserModal
                    description="user account"
                    doDelete={async () => {
                      await backend.deleteUser()
                      await signOut()
                    }}
                  />
                )
              }}
            >
              <span className="inline-block text">Delete this user account</span>
            </button>
            <span className="text my-auto">
              Once deleted, it will be gone forever. Please be certain.
            </span>
          </div>
        </div>
      </div>
      <div className="flex flex-col gap-settings-section-header">
        <h3 className="settings-subheading">Profile picture</h3>
        <label className="flex items-center cursor-pointer rounded-full overflow-clip h-profile-picture-large w-profile-picture-large hover:bg-frame transition-colors">
          <input type="file" className="hidden" accept="image/*" onChange={doUploadUserPicture} />
          <img
            src={user?.profilePicture ?? DefaultUserIcon}
            width={128}
            height={128}
            className="pointer-events-none"
          />
        </label>
        <span className="py-profile-picture-caption-y w-profile-picture-caption">
          Your profile picture should not be irrelevant, abusive or vulgar. It should not be a
          default image provided by Enso.
        </span>
      </div>
    </div>
  )
}
