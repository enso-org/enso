/** @file The input for viewing and changing the user's profile picture. */
import { useMutation, useQuery } from '@tanstack/react-query'

import DefaultUserIcon from '#/assets/default_user.svg'

import { backendMutationOptions, backendQueryOptions } from '#/hooks/backendHooks'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'

import { Form, HiddenFile } from '#/components/AriaComponents'
import { ProfilePicture } from '#/components/ProfilePicture/ProfilePicture'
import type Backend from '#/services/Backend'
import { StatelessSpinner } from '../../components/StatelessSpinner'

/** Props for a {@link ProfilePictureInput}. */
export interface ProfilePictureInputProps {
  readonly backend: Backend
}

/** The input for viewing and changing the user's profile picture. */
export default function ProfilePictureInput(props: ProfilePictureInputProps) {
  const { backend } = props
  const { data: user } = useQuery(backendQueryOptions(backend, 'usersMe', []))
  const { getText } = textProvider.useText()

  const uploadUserPicture = useMutation(backendMutationOptions(backend, 'uploadUserPicture'))

  return (
    <Form
      schema={(z) => z.object({ picture: z.instanceof(File) })}
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
              state="loading-medium"
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
          <HiddenFile autoSubmit name="picture" />
        </aria.Label>
      </FocusRing>

      <aria.Text className="w-profile-picture-caption py-profile-picture-caption-y">
        {getText('profilePictureWarning')}
      </aria.Text>

      <Form.FormError />
    </Form>
  )
}
