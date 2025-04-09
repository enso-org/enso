/** @file The input for viewing and changing the organization's profile picture. */
import { useMutation } from '@tanstack/react-query'

import { backendMutationOptions, useBackendQuery } from '#/hooks/backendHooks'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'

import { Form, HiddenFile } from '#/components/AriaComponents'
import { ProfilePicture } from '#/components/ProfilePicture'
import { StatelessSpinner } from '#/components/StatelessSpinner'
import type Backend from '#/services/Backend'

/** Props for a {@link OrganizationProfilePictureInput}. */
export interface OrganizationProfilePictureInputProps {
  readonly backend: Backend
}

/** The input for viewing and changing the organization's profile picture. */
export default function OrganizationProfilePictureInput(
  props: OrganizationProfilePictureInputProps,
) {
  const { backend } = props
  const { getText } = textProvider.useText()
  const { data: organization } = useBackendQuery(backend, 'getOrganization', [])

  const uploadOrganizationPicture = useMutation(
    backendMutationOptions(backend, 'uploadOrganizationPicture'),
  )

  return (
    <Form
      schema={(z) => z.object({ picture: z.instanceof(File) })}
      onSubmit={({ picture }) =>
        uploadOrganizationPicture.mutateAsync([{ fileName: picture.name }, picture])
      }
    >
      <FocusRing within>
        <aria.Label
          data-testid="organization-profile-picture-input"
          className="relative flex h-profile-picture-large w-profile-picture-large cursor-pointer items-center rounded-full transition-colors hover:bg-frame"
        >
          {uploadOrganizationPicture.isPending && (
            <StatelessSpinner
              state="loading-medium"
              className="absolute -inset-1"
              thickness={0.5}
            />
          )}

          <ProfilePicture
            picture={organization?.picture}
            name={organization?.name ?? ''}
            size="large"
            className="pointer-events-none h-full w-full"
          />

          <HiddenFile autoSubmit name="picture" />
        </aria.Label>
      </FocusRing>
      <aria.Text className="w-profile-picture-caption py-profile-picture-caption-y">
        {getText('organizationProfilePictureWarning')}
      </aria.Text>

      <Form.FormError />
    </Form>
  )
}
