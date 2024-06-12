/** @file The input for viewing and changing the organization's profile picture. */
import * as React from 'react'

import DefaultUserIcon from 'enso-assets/default_user.svg'

import * as backendHooks from '#/hooks/backendHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'

import type Backend from '#/services/Backend'

// =======================================
// === OrganizationProfilePictureInput ===
// =======================================

/** Props for a {@link OrganizationProfilePictureInput}. */
export interface OrganizationProfilePictureInputProps {
  readonly backend: Backend
}

/** The input for viewing and changing the organization's profile picture. */
export default function OrganizationProfilePictureInput(
  props: OrganizationProfilePictureInputProps
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
    <>
      <FocusRing within>
        <aria.Label className="flex h-profile-picture-large w-profile-picture-large cursor-pointer items-center overflow-clip rounded-full transition-colors hover:bg-frame">
          <img
            src={organization?.picture ?? DefaultUserIcon}
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
    </>
  )
}
