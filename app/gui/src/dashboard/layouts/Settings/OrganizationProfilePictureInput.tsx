/** @file The input for viewing and changing the organization's profile picture. */
import * as React from 'react'

import { useMutation } from '@tanstack/react-query'

import DefaultUserIcon from '#/assets/default_user.svg'

import { backendMutationOptions, useBackendQuery } from '#/hooks/backendHooks'
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
  props: OrganizationProfilePictureInputProps,
) {
  const { backend } = props
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { getText } = textProvider.useText()
  const { data: organization } = useBackendQuery(backend, 'getOrganization', [])

  const uploadOrganizationPicture = useMutation(
    backendMutationOptions(backend, 'uploadOrganizationPicture'),
  ).mutate

  const doUploadOrganizationPicture = (event: React.ChangeEvent<HTMLInputElement>) => {
    const image = event.target.files?.[0]
    if (image == null) {
      toastAndLog('noNewProfilePictureError')
    } else {
      uploadOrganizationPicture([{ fileName: image.name }, image])
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
    </>
  )
}
