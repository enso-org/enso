/** @file Modal for setting the organization name. */
import { Button } from '#/components/Button'
import { Dialog } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import { Result } from '#/components/Result'
import { Stepper } from '#/components/Stepper'
import { backendMutationOptions, backendQueryOptions } from '#/hooks/backendHooks'
import { ORGANIZATION_NAME_MAX_LENGTH, USER_GROUP_NAME_MAX_LENGTH } from '$/appUtils'
import { useBackends, useText } from '$/providers/react'
import type { GetText } from '$/providers/text'
import { useMutation, useSuspenseQuery } from '@tanstack/react-query'
import * as React from 'react'

/** A modal to set organization metadata. */
export function SetupOrganizationAfterSubscribe() {
  const { remoteBackend: backend } = useBackends()
  const { getText } = useText()
  const { data: organization } = useSuspenseQuery(
    backendQueryOptions(backend, 'getOrganization', []),
  )
  const { data: fetchedUserGroups } = useSuspenseQuery(
    backendQueryOptions(backend, 'listUserGroups', []),
  )

  const [hideModal, setHideModal] = React.useState(false)

  const updateOrganization = useMutation(backendMutationOptions(backend, 'updateOrganization'))
  const createDefaultUserGroup = useMutation(backendMutationOptions(backend, 'createUserGroup'))

  const shouldSetOrgName = organization?.name == null || organization.name === ''
  const shouldSetDefaultUserGroup = fetchedUserGroups.length === 0

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
    <Dialog
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
    </Dialog>
  )
}

/** Props for the SetOrganizationNameForm component. */
export interface SetOrganizationNameFormProps {
  readonly onSubmit: (name: string) => Promise<void>
}

// eslint-disable-next-line no-restricted-syntax, react-refresh/only-export-components
export const SET_ORGANIZATION_NAME_FORM_SCHEMA = (getText: GetText) =>
  Form.schema.object({
    name: Form.schema
      .string()
      .min(1, getText('arbitraryFieldRequired'))
      .max(ORGANIZATION_NAME_MAX_LENGTH, getText('arbitraryFieldTooLong')),
  })

/** Form for setting the organization name. */
export function SetOrganizationNameForm(props: SetOrganizationNameFormProps) {
  const { onSubmit } = props
  const { getText } = useText()

  return (
    <Form
      gap="medium"
      className="max-w-96"
      defaultValues={{ name: '' }}
      schema={SET_ORGANIZATION_NAME_FORM_SCHEMA(getText)}
      onSubmit={({ name }) => onSubmit(name)}
    >
      <Input
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

      <Form.Submit />

      <Form.FormError />
    </Form>
  )
}

/** Props for the CreateUserGroupForm component. */
export interface CreateUserGroupFormProps {
  readonly onSubmit: (name: string) => Promise<void>
}

/** Form for creating a user group. */
export function CreateUserGroupForm(props: CreateUserGroupFormProps) {
  const { onSubmit } = props
  const { getText } = useText()

  return (
    <Form
      schema={(z) => z.object({ groupName: z.string().min(1).max(USER_GROUP_NAME_MAX_LENGTH) })}
      gap="medium"
      className="max-w-96"
      defaultValues={{ groupName: '' }}
      onSubmit={({ groupName }) => onSubmit(groupName)}
    >
      <Input
        name="groupName"
        autoComplete="off"
        label={getText('groupNameSettingsInput')}
        description={getText('groupNameSettingsInputDescription', USER_GROUP_NAME_MAX_LENGTH)}
      />

      <Form.Submit />

      <Form.FormError />
    </Form>
  )
}
