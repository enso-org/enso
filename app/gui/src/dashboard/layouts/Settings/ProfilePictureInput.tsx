/** @file The input for viewing and changing the user's profile picture. */
import DefaultUserIcon from '#/assets/default_user.svg'
import { Label, Text } from '#/components/aria'
import { Form, HiddenFile } from '#/components/AriaComponents'
import { ProfilePicture } from '#/components/ProfilePicture/ProfilePicture'
import { StatelessSpinner } from '#/components/StatelessSpinner'
import FocusRing from '#/components/styled/FocusRing'
import { backendMutationOptions, backendQueryOptions } from '#/hooks/backendHooks'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { useMutation, useQuery } from '@tanstack/react-query'

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
      schema={(z) => z.object({ picture: z.instanceof(File) })}
      onSubmit={async ({ picture }) => {
        await uploadUserPicture.mutateAsync([{ fileName: picture.name }, picture])
      }}
    >
      <FocusRing within>
        <Label
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
          <HiddenFile autoSubmit name="picture" />
        </Label>
      </FocusRing>

      <Text className="w-profile-picture-caption py-profile-picture-caption-y">
        {getText('profilePictureWarning')}
      </Text>

      <Form.FormError />
    </Form>
  )
}
