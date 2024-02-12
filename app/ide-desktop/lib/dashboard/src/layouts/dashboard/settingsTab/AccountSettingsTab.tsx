/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'

import * as object from '#/utilities/object'

import ConfirmDeleteUserModal from '../ConfirmDeleteUserModal'

// =================
// === InfoEntry ===
// =================

/** Props for a transparent wrapper component. */
interface InternalTransparentWrapperProps {
  readonly children: React.ReactNode
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
  readonly children: [React.ReactNode, React.ReactNode]
}

/** Styled information display containing key and value. */
function InfoEntry(props: InternalInfoEntryProps) {
  const { children } = props
  const [name, value] = children
  return (
    <div className="flex gap-4.75">
      <span className="leading-5 w-12 h-8 py-1.25">{name}</span>
      <span className="grow font-bold leading-5 h-8 py-1.25">{value}</span>
    </div>
  )
}

// =============
// === Input ===
// =============

/** Props for an {@link Input}. */
interface InternalInputProps {
  readonly originalValue: string
  readonly placeholder?: string
  readonly onSubmit: (value: string) => void
  readonly onCancel?: () => void
}

/** A styled input. */
function Input(props: InternalInputProps) {
  const { originalValue, placeholder, onSubmit, onCancel } = props
  const ref = React.useRef<HTMLInputElement>(null)
  const cancelled = React.useRef(false)

  const onBlur = () => {
    if (cancelled.current) {
      onCancel?.()
    } else {
      onSubmit(ref.current?.value ?? '')
    }
  }

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
        cancelled.current = true
        break
      }
    }
  }

  return (
    <input
      ref={ref}
      className="rounded-full font-bold leading-5 w-full h-8 -mx-2 -my-1.25 px-2 py-1.25 bg-transparent hover:bg-frame-selected focus:bg-frame-selected transition-colors"
      type="text"
      size={1}
      defaultValue={originalValue}
      placeholder={placeholder}
      onBlur={onBlur}
      onKeyDown={onKeyDown}
    />
  )
}

// ==========================
// === AccountSettingsTab ===
// ==========================

/** Settings tab for viewing and editing account information. */
export default function AccountSettingsTab() {
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { setOrganization } = authProvider.useAuth()
  const { setModal } = modalProvider.useSetModal()
  const { backend } = backendProvider.useBackend()
  const { organization } = authProvider.useNonPartialUserSession()
  const [isEditingPassword, setIsEditingPassword] = React.useState(false)
  const [currentPassword, setCurrentPassword] = React.useState('')
  const [newPassword, setNewPassword] = React.useState('')
  const [confirmNewPassword, setConfirmNewPassword] = React.useState('')
  const nameInputRef = React.useRef<HTMLInputElement>(null)

  const doUpdateName = async (newName: string) => {
    const oldName = organization?.name ?? ''
    if (newName === oldName) {
      return
    } else {
      try {
        await backend.updateUser({ username: newName })
        setOrganization(object.merger({ name: newName }))
      } catch (error) {
        toastAndLog(null, error)
        if (nameInputRef.current) {
          nameInputRef.current.value = organization?.name ?? ''
        }
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
        setOrganization(newUser)
      } catch (error) {
        toastAndLog(null, error)
      }
    }
    // Reset selected files, otherwise the file input will do nothing if the same file is
    // selected again. While technically not undesired behavior, it is unintuitive for the user.
    event.target.value = ''
  }

  const cancelEditingPassword = React.useCallback(() => {
    setIsEditingPassword(false)
  }, [])

  return (
    <div className="flex gap-8">
      <div className="flex flex-col gap-8">
        <div className="flex flex-col gap-2.5">
          <h3 className="font-bold text-xl h-9.5 py-0.5">User Account</h3>
          <div className="flex flex-col">
            <InfoEntry>
              <Name>Name</Name>
              <Value>
                <Input originalValue={organization?.name ?? ''} onSubmit={doUpdateName} />
              </Value>
            </InfoEntry>
            <InfoEntry>
              <Name>Email</Name>
              <Value>{organization?.email ?? ''}</Value>
            </InfoEntry>
            {!isEditingPassword && (
              <InfoEntry>
                <Name>Password</Name>
                <Value>
                  <button
                    className="bg-frame-selected font-medium rounded-full h-6 py-px px-2 -my-px"
                    onClick={() => {
                      setIsEditingPassword(true)
                    }}
                  >
                    Change
                  </button>
                </Value>
              </InfoEntry>
            )}
            {isEditingPassword && (
              <InfoEntry>
                <Name>Current Password</Name>
                <Value>
                  <Input
                    originalValue=""
                    placeholder="Enter your current password"
                    onSubmit={setCurrentPassword}
                    onCancel={cancelEditingPassword}
                  />
                </Value>
              </InfoEntry>
            )}
            {isEditingPassword && (
              <InfoEntry>
                <Name>New Password</Name>
                <Value>
                  <Input
                    originalValue=""
                    placeholder="Enter your new password"
                    onSubmit={setNewPassword}
                    onCancel={cancelEditingPassword}
                  />
                </Value>
              </InfoEntry>
            )}
            {isEditingPassword && (
              <InfoEntry>
                <Name>Confirm New Password</Name>
                <Value>
                  <Input
                    originalValue=""
                    placeholder="Confirm your new password"
                    onSubmit={setConfirmNewPassword}
                    onCancel={cancelEditingPassword}
                  />
                </Value>
              </InfoEntry>
            )}
            {isEditingPassword && (
              <InfoEntry>
                <Name>Password</Name>
                <Value>
                  <button
                    disabled={
                      currentPassword === '' ||
                      newPassword === '' ||
                      confirmNewPassword === '' ||
                      newPassword !== confirmNewPassword
                    }
                    type="submit"
                    className="text-white bg-invite font-medium rounded-full h-6 py-px px-2 -my-px"
                    onClick={() => {
                      cancelEditingPassword()
                    }}
                  >
                    Change
                  </button>
                  <button
                    type="button"
                    className="bg-frame-selected font-medium rounded-full h-6 py-px px-2 -my-px"
                    onClick={cancelEditingPassword}
                  >
                    Cancel
                  </button>
                </Value>
              </InfoEntry>
            )}
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
              <span className="leading-5 h-6 py-px">Delete this user account</span>
            </button>
            <span className="leading-5 h-8 py-1.25">
              Once deleted, it will be gone forever. Please be certain.
            </span>
          </div>
        </div>
      </div>
      <div className="flex flex-col gap-2.5">
        <h3 className="font-bold text-xl h-9.5 py-0.5">Profile picture</h3>
        <label className="flex items-center cursor-pointer rounded-full overflow-clip h-32 w-32 hover:bg-frame transition-colors">
          <input type="file" className="hidden" accept="image/*" onChange={doUploadUserPicture} />
          <img src={organization?.profilePicture ?? DefaultUserIcon} width={128} height={128} />
        </label>
        <span className="py-1 w-64">
          Your organization&apos;s profile picture should not be irrelevant, abusive or vulgar. It
          should not be a default image provided by Enso.
        </span>
      </div>
    </div>
  )
}
