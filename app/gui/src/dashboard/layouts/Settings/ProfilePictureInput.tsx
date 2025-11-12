/** @file The input for viewing and changing the user's profile picture. */
import DefaultUserIcon from '#/assets/default_user.svg'
import * as aria from '#/components/aria'
import { Form } from '#/components/Form'
import { HiddenFile } from '#/components/Inputs/HiddenFile'
import { ProfilePicture } from '#/components/ProfilePicture/ProfilePicture'
import FocusRing from '#/components/styled/FocusRing'
import { backendMutationOptions, backendQueryOptions } from '#/hooks/backendHooks'
import { useText } from '$/providers/react'
import { useMutation, useQuery } from '@tanstack/react-query'
import type { Backend } from 'enso-common/src/services/Backend'
import { StatelessSpinner } from '../../components/StatelessSpinner'

/** Props for a {@link ProfilePictureInput}. */
export interface ProfilePictureInputProps {
  readonly backend: Backend
}

/** The input for viewing and changing the user's profile picture. */
export default function ProfilePictureInput(props: ProfilePictureInputProps) {
  const { backend } = props
  const { data: user } = useQuery(backendQueryOptions(backend, 'usersMe', []))
  const { getText } = useText()

  const uploadUserPicture = useMutation(backendMutationOptions(backend, 'uploadUserPicture'))

  return (
    <Form
      schema={(z) =>
        z.object({ picture: z.instanceof(File).refine((file) => file.type.startsWith('image/')) })
      }
      onSubmit={async ({ picture }) => {
        await uploadUserPicture.mutateAsync([{ fileName: picture.name }, picture])
      }}
    >
      <FocusRing within>
        <aria.Label
          data-testid="user-profile-picture-input"
          className="relative flex h-profile-picture-large w-profile-picture-large cursor-pointer items-center rounded-full transition-colors hover:bg-frame"
        >
          {uploadUserPicture.isPending && (
            <StatelessSpinner
              phase="loading-medium"
              className="absolute -inset-1"
              thickness={0.5}
            />
          )}

          <ProfilePicture
            picture={user?.profilePicture ?? DefaultUserIcon}
            name={user?.name ?? ''}
            size="large"
            className="pointer-events-none h-full w-full"
          />
          <HiddenFile autoSubmit name="picture" accept="image/*" />
        </aria.Label>
      </FocusRing>

      <aria.Text className="w-profile-picture-caption py-profile-picture-caption-y">
        {getText('profilePictureWarning')}
      </aria.Text>

      <Form.FormError />
    </Form>
  )
}
