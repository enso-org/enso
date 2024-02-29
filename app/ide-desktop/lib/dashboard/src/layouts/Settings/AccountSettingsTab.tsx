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
      className="rounded-full font-bold leading-5 w-full h-6 px-2 py-1.25 bg-transparent hover:bg-frame-selected focus:bg-frame-selected transition-colors placeholder-primary/30 invalid:border invalid:border-red-700"
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
    <div className="flex gap-8">
      <div className="flex flex-col gap-8">
        <div className="flex flex-col gap-2.5">
          <h3 className="font-bold text-xl h-9.5 py-0.5">{getText('userAccount')}</h3>
          <div className="flex flex-col">
            <div className="flex gap-4.75">
              <span className="leading-5 w-12 h-8 py-1.25">{getText('name')}</span>
              <span className="grow font-bold leading-5 h-8 py-1.25">
                <Input originalValue={user?.name ?? ''} onSubmit={doUpdateName} />
              </span>
            </div>
            <div className="flex gap-4.75">
              <span className="leading-5 w-12 h-8 py-1.25">{getText('email')}</span>
              <span className="grow font-bold leading-5 h-8 py-1.25">{user?.email ?? ''}</span>
            </div>
          </div>
        </div>
        {canChangePassword && (
          <div key={passwordFormKey}>
            <h3 className="font-bold text-xl h-9.5 py-0.5">{getText('changePassword')}</h3>
            <div className="flex gap-4.75">
              <span className="leading-5 w-36 h-8 py-1.25">{getText('currentPasswordLabel')}</span>
              <span className="grow font-bold leading-5 h-8 py-1.25">
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
            <div className="flex gap-4.75">
              <span className="leading-5 w-36 h-8 py-1.25">{getText('newPasswordLabel')}</span>
              <span className="grow font-bold leading-5 h-8 py-1.25">
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
            <div className="flex gap-4.75">
              <span className="leading-5 w-36 h-8 py-1.25">
                {getText('confirmNewPasswordLabel')}
              </span>
              <span className="grow font-bold leading-5 h-8 py-1.25">
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
            <div className="flex gap-2">
              <button
                disabled={
                  currentPassword === '' ||
                  newPassword === '' ||
                  confirmNewPassword === '' ||
                  newPassword !== confirmNewPassword ||
                  !validation.PASSWORD_REGEX.test(newPassword)
                }
                type="submit"
                className="text-white bg-invite font-medium rounded-full h-6 py-px px-2 -my-px disabled:opacity-50"
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
                className="bg-frame-selected font-medium rounded-full h-6 py-px px-2 -my-px"
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
        <div className="flex flex-col gap-2.5 rounded-2.5xl border-2 border-danger px-4 pt-2.25 pb-3.75">
          <h3 className="text-danger font-bold text-xl h-9.5 py-0.5">{getText('dangerZone')}</h3>
          <div className="flex gap-2">
            <button
              className="rounded-full bg-danger text-inversed px-2 py-1"
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
              <span className="leading-5 h-6 py-px">{getText('deleteUserAccountButtonLabel')}</span>
            </button>
            <span className="leading-5 h-8 py-1.25">{getText('deleteUserAccountWarning')}</span>
          </div>
        </div>
      </div>
      <div className="flex flex-col gap-2.5">
        <h3 className="font-bold text-xl h-9.5 py-0.5">{getText('profilePicture')}</h3>
        <label className="flex items-center cursor-pointer rounded-full overflow-clip h-32 w-32 hover:bg-frame transition-colors">
          <input type="file" className="hidden" accept="image/*" onChange={doUploadUserPicture} />
          <img
            src={user?.profilePicture ?? DefaultUserIcon}
            width={128}
            height={128}
            className="pointer-events-none"
          />
        </label>
        <span className="py-1 w-64">{getText('profilePictureWarning')}</span>
      </div>
    </div>
  )
}
