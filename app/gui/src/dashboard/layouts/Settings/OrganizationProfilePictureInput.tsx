/** @file The input for viewing and changing the organization's profile picture. */
import * as aria from '#/components/aria'
import { Form } from '#/components/Form'
import { HiddenFile } from '#/components/Inputs/HiddenFile'
import { ProfilePicture } from '#/components/ProfilePicture'
import { StatelessSpinner } from '#/components/StatelessSpinner'
import FocusRing from '#/components/styled/FocusRing'
import { backendMutationOptions, backendQueryOptions } from '#/hooks/backendHooks'
import { useText } from '$/providers/react'
import { useMutation, useQuery } from '@tanstack/react-query'
import type { Backend } from 'enso-common/src/services/Backend'

/** Props for a {@link OrganizationProfilePictureInput}. */
export interface OrganizationProfilePictureInputProps {
  readonly backend: Backend
}

/** The input for viewing and changing the organization's profile picture. */
export default function OrganizationProfilePictureInput(
  props: OrganizationProfilePictureInputProps,
) {
  const { backend } = props
  const { getText } = useText()
  const { data: organization } = useQuery(backendQueryOptions(backend, 'getOrganization', []))

  const uploadOrganizationPicture = useMutation(
    backendMutationOptions(backend, 'uploadOrganizationPicture'),
  )

  return (
    <Form
      schema={(z) =>
        z.object({ picture: z.instanceof(File).refine((file) => file.type.startsWith('image/')) })
      }
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
              phase="loading-medium"
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

          <HiddenFile autoSubmit name="picture" accept="image/*" />
        </aria.Label>
      </FocusRing>
      <aria.Text className="w-profile-picture-caption py-profile-picture-caption-y">
        {getText('organizationProfilePictureWarning')}
      </aria.Text>

      <Form.FormError />
    </Form>
  )
}
