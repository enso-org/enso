/** @file Settings tab for viewing and changing profile picture. */
import * as React from 'react'

import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'

import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'

// =====================================
// === ProfilePictureSettingsSection ===
// =====================================

/** Settings tab for viewing and changing profile picture. */
export default function ProfilePictureSettingsSection() {
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { setUser } = authProvider.useAuth()
  const { backend } = backendProvider.useBackend()
  const { user } = authProvider.useNonPartialUserSession()

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
    <FocusArea direction="vertical">
      {(ref, innerProps) => (
        <div ref={ref} className="flex flex-col gap-settings-section-header" {...innerProps}>
          <h3 className="settings-subheading">Profile picture</h3>
          <FocusRing within>
            <label className="flex h-profile-picture-large w-profile-picture-large cursor-pointer items-center overflow-clip rounded-full transition-colors hover:bg-frame">
              <img
                src={user?.profilePicture ?? DefaultUserIcon}
                width={128}
                height={128}
                className="pointer-events-none"
              />
              <input type="file" className="w" accept="image/*" onChange={doUploadUserPicture} />
            </label>
          </FocusRing>
          <span className="w-profile-picture-caption py-profile-picture-caption-y">
            Your profile picture should not be irrelevant, abusive or vulgar. It should not be a
            default image provided by Enso.
          </span>
        </div>
      )}
    </FocusArea>
  )
}
