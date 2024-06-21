/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as backendHooks from '#/hooks/backendHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'
import SettingsSection from '#/components/styled/settings/SettingsSection'

import type Backend from '#/services/Backend'

// =================================================
// === OrganizationProfilePictureSettingsSection ===
// =================================================

/** Props for a {@link OrganizationProfilePictureSettingsSection}. */
export interface OrganizationProfilePictureSettingsSectionProps {
  readonly backend: Backend
}

/** Settings tab for viewing and editing organization information. */
export default function OrganizationProfilePictureSettingsSection(
  props: OrganizationProfilePictureSettingsSectionProps
) {
  const { backend } = props
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { getText } = textProvider.useText()
  const organization = backendHooks.useBackendGetOrganization(backend)

  const uploadOrganizationPictureMutation = backendHooks.useBackendMutation(
    backend,
    'uploadOrganizationPicture'
  )

  const doUploadOrganizationPicture = async (event: React.ChangeEvent<HTMLInputElement>) => {
    const image = event.target.files?.[0]
    if (image == null) {
      toastAndLog('noNewProfilePictureError')
    } else {
      try {
        await uploadOrganizationPictureMutation.mutateAsync([{ fileName: image.name }, image])
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
            src={organization?.picture ?? DefaultUserIcon}
            className="pointer-events-none h-full w-full"
          />
          <aria.Input
            type="file"
            className="focus-child w-0"
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
