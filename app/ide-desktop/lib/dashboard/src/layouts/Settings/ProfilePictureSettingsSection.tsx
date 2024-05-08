/** @file Settings tab for viewing and changing profile picture. */
import * as React from 'react'

import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'
import SettingsSection from '#/components/styled/settings/SettingsSection'

// =====================================
// === ProfilePictureSettingsSection ===
// =====================================

/** Settings tab for viewing and changing profile picture. */
export default function ProfilePictureSettingsSection() {
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { setUser } = authProvider.useAuth()
  const backend = backendProvider.useRemoteBackendStrict()
  const { user } = authProvider.useNonPartialUserSession()
  const { getText } = textProvider.useText()

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
    <SettingsSection title={getText('profilePicture')}>
      <FocusRing within>
        <aria.Label className="flex h-profile-picture-large w-profile-picture-large cursor-pointer items-center overflow-clip rounded-full transition-colors hover:bg-frame">
          <img
            src={user?.profilePicture ?? DefaultUserIcon}
            width={128}
            height={128}
            className="pointer-events-none"
          />
          <aria.Input
            type="file"
            className="focus-child w"
            accept="image/*"
            onChange={doUploadUserPicture}
          />
        </aria.Label>
      </FocusRing>
      <aria.Text className="w-profile-picture-caption py-profile-picture-caption-y">
        {getText('profilePictureWarning')}
      </aria.Text>
    </SettingsSection>
  )
}
