/** @file Modal for setting the organization name. */
import * as React from 'react'

import { useMutation, useSuspenseQueries } from '@tanstack/react-query'
import * as router from 'react-router'

import { backendMutationOptions, backendQueryOptions } from '#/hooks/backendHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import type { GetText } from '#/providers/TextProvider'
import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

import { Button } from '#/components/AriaComponents'
import { Result } from '#/components/Result'
import { Stepper } from '#/components/Stepper'
import * as backendModule from '#/services/Backend'
import type RemoteBackend from '../services/RemoteBackend'

const PLANS_TO_SPECIFY_ORG_NAME = [backendModule.Plan.team, backendModule.Plan.enterprise]

/**
 * Modal for setting the organization name.
 * Shows up when the user is on the team plan and the organization name is the default.
 */
export function SetupOrganizationAfterSubscribe() {
  const backend = backendProvider.useRemoteBackend()

  const session = authProvider.useFullUserSession()
  const { user } = session
  const { isOrganizationAdmin, userId, plan = backendModule.Plan.free } = user

  const shouldShowModal = PLANS_TO_SPECIFY_ORG_NAME.includes(plan) && isOrganizationAdmin

  if (shouldShowModal) {
    return <SetupOrganizationAfterSubscribeInternal userId={userId} backend={backend} />
  }

  return <router.Outlet context={session} />
}

/**
 * Props for the SetupOrganizationAfterSubscribeInternal component.
 * @param props - The props for the component.
 * @returns The component.
 */
interface SetupOrganizationAfterSubscribeInternalProps {
  readonly userId: string
  readonly backend: RemoteBackend
}

/**
 * Internal implementation of the SetupOrganizationAfterSubscribe modal.
 * @param props - The props for the component.
 * @returns The component.
 */
function SetupOrganizationAfterSubscribeInternal(
  props: SetupOrganizationAfterSubscribeInternalProps,
) {
  const { backend } = props

  const { getText } = textProvider.useText()
  const session = authProvider.useFullUserSession()

  const { organizationName, userGroupsCount } = useSuspenseQueries({
    queries: [
      backendQueryOptions(backend, 'getOrganization', []),
      backendQueryOptions(backend, 'listUserGroups', []),
    ],
    combine: ([organizationQuery, userGroupsQuery]) => ({
      // Null is used to indicate that the user is not an admin of an organization,
      // Or organization info has not yet been created, This means that the dialog
      // should not be shown.
      organizationName: organizationQuery.data?.name ?? null,
      userGroupsCount: userGroupsQuery.data.length,
    }),
  })

  const [hideModal, setHideModal] = React.useState(false)

  const updateOrganization = useMutation(backendMutationOptions(backend, 'updateOrganization'))
  const createDefaultUserGroup = useMutation(backendMutationOptions(backend, 'createUserGroup'))

  const shouldSetOrgName = organizationName === ''
  const shouldSetDefaultUserGroup = userGroupsCount === 0

  const steps = [
    {
      title: getText('intro'),
      component: ({ nextStep }: { readonly nextStep: () => void }) => (
        <Result
          status="info"
          title={getText('setupOrganization')}
          subtitle={getText('setupOrganizationDescription')}
        >
          <Button onPress={nextStep} className="mx-auto">
            {getText('next')}
          </Button>
        </Result>
      ),
    } as const,
  ]

  if (shouldSetOrgName) {
    steps.push({
      title: getText('setOrgNameTitle'),
      component: ({ nextStep }) => (
        <SetOrganizationNameForm
          onSubmit={async (name) => {
            await updateOrganization.mutateAsync([{ name }])
            nextStep()
          }}
        />
      ),
    })
  }

  if (shouldSetDefaultUserGroup) {
    steps.push({
      title: getText('setDefaultUserGroup'),
      component: ({ nextStep }) => (
        <CreateUserGroupForm
          onSubmit={async (name) => {
            await createDefaultUserGroup.mutateAsync([{ name }])
            nextStep()
          }}
        />
      ),
    })
  }

  const shouldShowModal = steps.length > 1 && !hideModal

  const { stepperState } = Stepper.useStepperState({
    steps: steps.length,
    defaultStep: 0,
    onCompleted: () => {
      setHideModal(true)
    },
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
          renderStep={(stepProps) => (
            <Stepper.Step {...stepProps} title={steps[stepProps.index]?.title ?? ''} />
          )}
        >
          {({ currentStep, nextStep }) => <>{steps[currentStep]?.component({ nextStep })}</>}
        </Stepper>
      </ariaComponents.Dialog>

      <router.Outlet context={session} />
    </>
  )
}

/** Props for the SetOrganizationNameForm component. */
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
      .max(ORGANIZATION_NAME_MAX_LENGTH, getText('arbitraryFieldTooLong')),
  })

/** Form for setting the organization name. */
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

      <ariaComponents.Form.Submit />

      <ariaComponents.Form.FormError />
    </ariaComponents.Form>
  )
}

/** Props for the CreateUserGroupForm component. */
export interface CreateUserGroupFormProps {
  readonly onSubmit: (name: string) => Promise<void>
}

/** Form for creating a user group. */
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

      <ariaComponents.Form.FormError />
    </ariaComponents.Form>
  )
}
