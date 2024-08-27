/** @file Modal for setting the organization name. */
import * as React from 'react'

import { useMutation, useSuspenseQuery } from '@tanstack/react-query'
import * as router from 'react-router'

import { backendMutationOptions } from '#/hooks/backendHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import type { GetText } from '#/providers/TextProvider'
import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

import * as backendModule from '#/services/Backend'

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
    backendMutationOptions(backend, 'updateOrganization'),
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
        <SetOrganizationNameForm
          onSubmit={async (name) => {
            await updateOrganization([{ name }])
          }}
        />
      </ariaComponents.Dialog>

      <router.Outlet context={session} />
    </>
  )
}

/**
 * Props for the SetOrganizationNameForm component.
 */
export interface SetOrganizationNameFormProps {
  readonly onSubmit: (name: string) => Promise<void>
}

export const ORGANIZATION_NAME_MAX_LENGTH = 64

// eslint-disable-next-line no-restricted-syntax
export const SET_ORGANIZATION_NAME_FORM_SCHEMA = (getText: GetText) =>
  ariaComponents.Form.schema.object({
    name: ariaComponents.Form.schema
      .string()
      .min(1, getText('arbitraryFieldRequired'))
      // eslint-disable-next-line @typescript-eslint/no-magic-numbers
      .max(ORGANIZATION_NAME_MAX_LENGTH, getText('arbitraryFieldTooLong')),
  })

/**
 * Form for setting the organization name.
 */
export function SetOrganizationNameForm(props: SetOrganizationNameFormProps) {
  const { onSubmit } = props
  const { getText } = textProvider.useText()

  return (
    <ariaComponents.Form
      gap="medium"
      method="dialog"
      defaultValues={{ name: '' }}
      schema={SET_ORGANIZATION_NAME_FORM_SCHEMA(getText)}
      onSubmit={({ name }) => onSubmit(name)}
    >
      <ariaComponents.Input
        name="name"
        autoFocus
        inputMode="text"
        autoComplete="off"
        label={getText('organizationNameSettingsInput')}
        placeholder={getText('organizationNamePlaceholder')}
        description={getText(
          'organizationNameSettingsInputDescription',
          ORGANIZATION_NAME_MAX_LENGTH,
        )}
      />

      <ariaComponents.Form.FormError />

      <ariaComponents.Form.Submit />
    </ariaComponents.Form>
  )
}
