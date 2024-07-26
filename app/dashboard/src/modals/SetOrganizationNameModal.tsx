/** @file Modal for setting the organization name. */
import * as React from 'react'

import { useMutation, useSuspenseQuery } from '@tanstack/react-query'
import * as router from 'react-router'

import { useBackendMutationOptions } from '#/hooks/backendHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

import * as backendModule from '#/services/Backend'

import * as tailwindMerge from '#/utilities/tailwindMerge'

// =================
// === Constants ===
// =================

const PLANS_TO_SPECIFY_ORG_NAME = [backendModule.Plan.team, backendModule.Plan.enterprise]

// ================================
// === SetOrganizationNameModal ===
// ================================

/** Modal for setting the organization name.
 * Shows up when the user is on the team plan and the organization name is the default. */
export function SetOrganizationNameModal() {
  const { getText } = textProvider.useText()

  const backend = backendProvider.useRemoteBackendStrict()
  const { session } = authProvider.useAuth()

  const user = session != null && 'user' in session ? session.user : null
  const userId = user?.userId ?? null
  const userPlan = user?.plan ?? null

  const { data: organizationName } = useSuspenseQuery({
    queryKey: ['organization', userId],
    queryFn: () => backend.getOrganization().catch(() => null),
    staleTime: Infinity,
    select: (data) => data?.name ?? '',
  })

  const updateOrganization = useMutation(
    useBackendMutationOptions(backend, 'updateOrganization'),
  ).mutateAsync

  const shouldShowModal =
    userPlan != null && PLANS_TO_SPECIFY_ORG_NAME.includes(userPlan) && organizationName === ''

  return (
    <>
      <ariaComponents.Dialog
        title={getText('setOrgNameTitle')}
        isDismissable={false}
        isKeyboardDismissDisabled
        hideCloseButton
        modalProps={{ isOpen: shouldShowModal }}
      >
        <ariaComponents.Form
          gap="medium"
          defaultValues={{ name: '' }}
          schema={ariaComponents.Form.useFormSchema((z) =>
            z.object({
              name: z
                .string()
                .min(1, getText('arbitraryFieldRequired'))
                // eslint-disable-next-line @typescript-eslint/no-magic-numbers
                .max(255, getText('arbitraryFieldTooLong')),
            }),
          )}
          onSubmit={({ name }) => updateOrganization([{ name }])}
        >
          {({ register, formState }) => {
            return (
              <>
                <aria.TextField
                  autoFocus
                  inputMode="text"
                  autoComplete="off"
                  className="flex w-full flex-col"
                  {...register('name')}
                >
                  <aria.Label className="mb-1 ml-0.5 block text-sm">
                    {getText('organization')}
                  </aria.Label>

                  <aria.Input
                    className={(values) =>
                      tailwindMerge.twMerge(
                        'rounded-md border border-gray-300 p-1.5 text-sm transition-[outline]',
                        (values.isFocused || values.isFocusVisible) &&
                          'outline outline-2 outline-primary',
                        values.isInvalid && 'border-red-500 outline-red-500',
                      )
                    }
                  />

                  <aria.FieldError className="text-sm text-red-500">
                    {formState.errors.name?.message}
                  </aria.FieldError>
                </aria.TextField>

                <ariaComponents.Form.FormError />

                <ariaComponents.Form.Submit />
              </>
            )
          }}
        </ariaComponents.Form>
      </ariaComponents.Dialog>
      <router.Outlet context={session} />
    </>
  )
}
