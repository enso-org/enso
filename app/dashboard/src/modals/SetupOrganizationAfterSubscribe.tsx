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

import { Button } from '#/components/AriaComponents'
import { Result } from '#/components/Result'
import { Stepper } from '#/components/Stepper'
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
export function SetupOrganizationAfterSubscribe() {
  const { getText } = textProvider.useText()

  const backend = backendProvider.useRemoteBackendStrict()
  const { session } = authProvider.useAuth()

  const user = session != null && 'user' in session ? session.user : null
  const userId = user?.userId ?? null
  const userPlan = user?.plan ?? backendModule.Plan.free

  const { data: organizationName } = useSuspenseQuery({
    queryKey: ['organization', userId],
    queryFn: () => backend.getOrganization().catch(() => null),
    staleTime: Infinity,
    select: (data) => data?.name ?? '',
  })

  const { data: hasUserGroups } = useSuspenseQuery({
    queryKey: ['userGroups', userId],
    queryFn: () => backend.listUserGroups().catch(() => null),
    staleTime: Infinity,
    select: (data) => data?.length ?? 0,
  })

  const updateOrganization = useMutation(
    backendMutationOptions(backend, 'updateOrganization', {
      meta: { invalidates: [['organization', userId]] },
    }),
  )
  const createDefaultUserGroup = useMutation(
    backendMutationOptions(backend, 'createUserGroup', {
      meta: { invalidates: [['userGroups', userId]] },
    }),
  )

  const shouldSetOrgName = PLANS_TO_SPECIFY_ORG_NAME.includes(userPlan) && organizationName === ''
  const shouldSetDefaultUserGroup =
    PLANS_TO_SPECIFY_ORG_NAME.includes(userPlan) && hasUserGroups === 0
  const shouldShowModal = shouldSetOrgName || shouldSetDefaultUserGroup

  const { stepperState } = Stepper.useStepperState({
    steps: 3,
    defaultStep: 0,
  })

  return (
    <>
      <ariaComponents.Dialog
        title={getText('setupOrganization')}
        isDismissable={false}
        isKeyboardDismissDisabled
        hideCloseButton
        size="xxxlarge"
        padding="xlarge"
        modalProps={{ isOpen: shouldShowModal }}
      >
        <Stepper
          state={stepperState}
          renderStep={(props) => (
            <>
              {props.index === 0 && <Stepper.Step {...props} title={getText('intro')} />}
              {props.index === 1 && <Stepper.Step {...props} title={getText('setOrgNameTitle')} />}
              {props.index === 2 && (
                <Stepper.Step {...props} title={getText('setDefaultUserGroup')} />
              )}
            </>
          )}
        >
          {({ currentStep, nextStep }) => (
            <>
              {currentStep === 0 && (
                <Result
                  status="info"
                  title={getText('setupOrganization')}
                  subtitle={getText('setupOrganizationDescription')}
                >
                  <Button onPress={nextStep} className="mx-auto">
                    {getText('next')}
                  </Button>
                </Result>
              )}

              {currentStep === 1 && (
                <SetOrganizationNameForm
                  onSubmit={async (name) => {
                    await updateOrganization.mutateAsync([{ name }])
                    nextStep()
                  }}
                />
              )}

              {currentStep === 2 && (
                <CreateUserGroupForm
                  onSubmit={async (name) => {
                    await createDefaultUserGroup.mutateAsync([{ name }])
                  }}
                />
              )}
            </>
          )}
        </Stepper>
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
      className="max-w-96"
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

/**
 *
 */
export interface CreateUserGroupFormProps {
  readonly onSubmit: (name: string) => Promise<void>
}

/**
 * Form for creating a user group.
 */
export function CreateUserGroupForm(props: CreateUserGroupFormProps) {
  const { onSubmit } = props
  const { getText } = textProvider.useText()

  const defaultUserGroupMaxLength = 64

  return (
    <ariaComponents.Form
      schema={(z) => z.object({ groupName: z.string().min(1).max(defaultUserGroupMaxLength) })}
      gap="medium"
      className="max-w-96"
      defaultValues={{ groupName: '' }}
      onSubmit={({ groupName }) => onSubmit(groupName)}
    >
      <ariaComponents.Input
        name="groupName"
        autoComplete="off"
        label={getText('groupNameSettingsInput')}
        description={getText('groupNameSettingsInputDescription', defaultUserGroupMaxLength)}
      />

      <ariaComponents.Form.Submit />
    </ariaComponents.Form>
  )
}
