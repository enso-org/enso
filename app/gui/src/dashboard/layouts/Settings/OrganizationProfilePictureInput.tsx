/** @file The input for viewing and changing the organization's profile picture. */
import { useMutation } from '@tanstack/react-query'

import DefaultUserIcon from '#/assets/default_user.svg'

import { backendMutationOptions, useBackendQuery } from '#/hooks/backendHooks'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'

import { Form, HiddenFile } from '#/components/AriaComponents'
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
  const { getText } = textProvider.useText()
  const { data: organization } = useBackendQuery(backend, 'getOrganization', [])

  const uploadOrganizationPicture = useMutation(
    backendMutationOptions(backend, 'uploadOrganizationPicture'),
  )

  const form = Form.useForm({
    schema: (z) => z.object({ picture: z.instanceof(File) }),
    onSubmit: async ({ picture }) => {
      await uploadOrganizationPicture.mutateAsync([{ fileName: picture.name }, picture])
    },
  })

  return (
    <Form form={form}>
      <FocusRing within>
        <aria.Label
          data-testid="organization-profile-picture-input"
          className="flex h-profile-picture-large w-profile-picture-large cursor-pointer items-center overflow-clip rounded-full transition-colors hover:bg-frame"
        >
          <img
            src={organization?.picture ?? DefaultUserIcon}
            className="pointer-events-none h-full w-full"
          />
          <HiddenFile autoSubmit form={form} name="picture" />
        </aria.Label>
      </FocusRing>
      <aria.Text className="w-profile-picture-caption py-profile-picture-caption-y">
        {getText('organizationProfilePictureWarning')}
      </aria.Text>
    </Form>
  )
}
