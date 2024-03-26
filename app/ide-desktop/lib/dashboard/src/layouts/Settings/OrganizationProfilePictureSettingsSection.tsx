/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'

import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'

import type * as backendModule from '#/services/Backend'

// =================================================
// === OrganizationProfilePictureSettingsSection ===
// =================================================

/** Props for a {@link OrganizationProfilePictureSettingsSection}. */
export interface OrganizationProfilePictureSettingsSectionProps {
  readonly organization: backendModule.OrganizationInfo
  readonly setOrganization: React.Dispatch<React.SetStateAction<backendModule.OrganizationInfo>>
}

/** Settings tab for viewing and editing organization information. */
export default function OrganizationProfilePictureSettingsSection(
  props: OrganizationProfilePictureSettingsSectionProps
) {
  const { organization, setOrganization } = props
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { backend } = backendProvider.useBackend()

  const doUploadOrganizationPicture = async (event: React.ChangeEvent<HTMLInputElement>) => {
    const image = event.target.files?.[0]
    if (image == null) {
      toastAndLog('Could not upload a new profile picture because no image was found')
    } else {
      try {
        const newOrganization = await backend.uploadOrganizationPicture(
          { fileName: image.name },
          image
        )
        setOrganization(newOrganization)
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
                src={organization.picture ?? DefaultUserIcon}
                width={128}
                height={128}
                className="pointer-events-none"
              />
              <input
                type="file"
                className="focus-child w"
                accept="image/*"
                onChange={doUploadOrganizationPicture}
              />
            </label>
          </FocusRing>
          <span className="w-profile-picture-caption py-profile-picture-caption-y">
            Your organization&apos;s profile picture should not be irrelevant, abusive or vulgar. It
            should not be a default image provided by Enso.
          </span>
        </div>
      )}
    </FocusArea>
  )
}
