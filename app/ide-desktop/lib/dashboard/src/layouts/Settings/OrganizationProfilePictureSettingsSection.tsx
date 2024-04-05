/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'
import SettingsSection from '#/components/styled/settings/SettingsSection'

import type * as backendModule from '#/services/Backend'

// =================================================
// === OrganizationProfilePictureSettingsSection ===
// =================================================

/** Props for a {@link OrganizationProfilePictureSettingsSection}. */
export interface OrganizationProfilePictureSettingsSectionProps {
  readonly organization: backendModule.SmartOrganization | null
  readonly setOrganizationInfo: React.Dispatch<React.SetStateAction<backendModule.OrganizationInfo>>
}

/** Settings tab for viewing and editing organization information. */
export default function OrganizationProfilePictureSettingsSection(
  props: OrganizationProfilePictureSettingsSectionProps
) {
  const { organization, setOrganizationInfo } = props
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { getText } = textProvider.useText()

  const doUploadOrganizationPicture = async (event: React.ChangeEvent<HTMLInputElement>) => {
    const image = event.target.files?.[0]
    if (image == null) {
      toastAndLog('noNewProfilePictureError')
    } else if (organization == null) {
      // Ignored.
    } else {
      try {
        const newOrganization = await organization.uploadPicture({ fileName: image.name }, image)
        setOrganizationInfo(newOrganization)
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
            src={organization?.value?.picture ?? DefaultUserIcon}
            width={128}
            height={128}
            className="pointer-events-none"
          />
          <aria.Input
            type="file"
            className="focus-child w"
            accept="image/*"
            onChange={doUploadOrganizationPicture}
          />
        </aria.Label>
      </FocusRing>
      <aria.Text className="w-profile-picture-caption py-profile-picture-caption-y">
        {getText('organizationProfilePictureWarning')}
      </aria.Text>
    </SettingsSection>
  )
}
