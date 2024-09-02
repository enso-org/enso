/**
 * @file
 * Setup page
 */
import * as React from 'react'

import { useMutation, useSuspenseQuery } from '@tanstack/react-query'
import { Navigate, useSearchParams } from 'react-router-dom'
import invariant from 'tiny-invariant'

import type * as text from 'enso-common/src/text'

import ArrowRight from '#/assets/arrow_right.svg'

import { DASHBOARD_PATH, LOGIN_PATH } from '#/appUtils'

import { useIsFirstRender } from '#/hooks/mountHooks'

import { useAuth, UserSessionType } from '#/providers/AuthProvider'
import { useRemoteBackendStrict } from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import Page from '#/components/Page'
import * as stepper from '#/components/Stepper'

import { ORGANIZATION_NAME_MAX_LENGTH } from '#/modals/SetupOrganizationAfterSubscribe'

import { backendMutationOptions } from '#/hooks/backendHooks'
import { InviteUsersForm } from '#/modals/InviteUsersModal'
import { PlanSelector } from '#/modules/payments'
import { Plan } from '#/services/Backend'

/**
 * Step in the setup process
 */
interface Step {
  readonly title: text.TextId
  readonly description?: text.TextId
  readonly text?: text.TextId
  readonly component?: React.ComponentType<Context>
  readonly canSkip?: boolean | ((context: Context) => boolean)
  readonly hideNext?: boolean | ((context: Context) => boolean)
  readonly hidePrevious?: boolean | ((context: Context) => boolean)
  readonly ignore?: (context: Context) => boolean
}

/**
 * Context for the setup process
 */
interface Context {
  readonly session: ReturnType<typeof useAuth>['session']
  readonly plan: Plan
  readonly goToNextStep: () => void
  readonly goToPreviousStep: () => void
}

const BASE_STEPS: Step[] = [
  {
    title: 'setUsername',
    text: 'setUsernameDescription',
    hideNext: true,
    /**
     * Step component
     */
    component: function SetUsernameStep({ session, goToNextStep }) {
      const { setUsername } = useAuth()
      const { getText } = textProvider.useText()

      const defaultName = session && 'user' in session ? session.user.name : ''

      return (
        <ariaComponents.Form
          className="max-w-96"
          schema={(z) =>
            z.object({
              username: z
                .string()
                .min(3)
                /* eslint-disable-next-line @typescript-eslint/no-magic-numbers */
                .max(48),
            })
          }
          defaultValues={{ username: defaultName }}
          onSubmit={({ username }) => {
            if (username === defaultName) {
              goToNextStep()
              return
            } else {
              return setUsername(username).then(() => {
                goToNextStep()
              })
            }
          }}
        >
          <ariaComponents.Input
            name="username"
            label={getText('userNameSettingsInput')}
            placeholder={getText('usernamePlaceholder')}
            description="Minimum 3 characters, maximum 48 characters"
          />

          <ariaComponents.Form.Submit variant="primary">
            {getText('next')}
          </ariaComponents.Form.Submit>

          <ariaComponents.Form.FormError />
        </ariaComponents.Form>
      )
    },
  },
  {
    title: 'choosePlan',
    text: 'choosePlanDescription',
    ignore: ({ session }) =>
      session && 'user' in session ? !session.user.isOrganizationAdmin : true,
    canSkip: ({ plan }) => plan === Plan.free,
    hideNext: ({ plan }) => plan === Plan.free,
    /**
     * Step component
     */
    component: function ChoosePlanStep({ goToNextStep, plan, session }) {
      const isOrganizationAdmin =
        session && 'user' in session ? session.user.isOrganizationAdmin : false

      return (
        <PlanSelector
          userPlan={plan}
          isOrganizationAdmin={isOrganizationAdmin}
          hasTrial={plan === Plan.free}
          onSubscribeSuccess={goToNextStep}
        />
      )
    },
  },
  {
    title: 'setOrgNameTitle',
    text: 'setOrgNameDescription',
    ignore: (context) => {
      const isOrganizationAdmin =
        context.session && 'user' in context.session ?
          context.session.user.isOrganizationAdmin
        : false

      return context.plan === Plan.free || context.plan === Plan.solo || !isOrganizationAdmin
    },
    hideNext: true,
    hidePrevious: true,
    /**
     * Step component
     */
    component: function SetOrganizatioNameStep({ goToNextStep, goToPreviousStep }) {
      const { getText } = textProvider.useText()
      const remoteBackend = useRemoteBackendStrict()

      const { data: defaultOrgName } = useSuspenseQuery({
        queryKey: ['organization'],
        queryFn: () => remoteBackend.getOrganization(),
        select: (data) => data?.name ?? '',
      })

      return (
        <ariaComponents.Form
          schema={(z) =>
            z.object({ organizationName: z.string().min(1).max(ORGANIZATION_NAME_MAX_LENGTH) })
          }
          defaultValues={{ organizationName: defaultOrgName }}
          className="max-w-96"
          onSubmit={({ organizationName }) => {
            if (organizationName !== defaultOrgName) {
              // eslint-disable-next-line no-restricted-syntax
              return remoteBackend.updateOrganization({ name: organizationName })
            }
          }}
          onSubmitSuccess={goToNextStep}
        >
          <ariaComponents.Input
            name="organizationName"
            autoComplete="off"
            label={getText('organizationNameSettingsInput')}
            description={getText(
              'organizationNameSettingsInputDescription',
              ORGANIZATION_NAME_MAX_LENGTH,
            )}
          />

          <ariaComponents.ButtonGroup align="start">
            <ariaComponents.Button variant="outline" onPress={goToPreviousStep}>
              {getText('back')}
            </ariaComponents.Button>

            <ariaComponents.Form.Submit variant="primary">
              {getText('next')}
            </ariaComponents.Form.Submit>
          </ariaComponents.ButtonGroup>

          <ariaComponents.Form.FormError />
        </ariaComponents.Form>
      )
    },
  },
  {
    title: 'inviteUsers',
    text: 'inviteUsersDescription',
    ignore: (context) => {
      const isOrganizationAdmin =
        context.session && 'user' in context.session ?
          context.session.user.isOrganizationAdmin
        : false

      return context.plan === Plan.free || context.plan === Plan.solo || !isOrganizationAdmin
    },
    hideNext: true,
    hidePrevious: true,
    /**
     * Step component
     */
    component: function InviteUsersStep({ goToNextStep, goToPreviousStep }) {
      const { getText } = textProvider.useText()

      return (
        <div className="max-w-96">
          <InviteUsersForm
            onSubmitted={() => {
              goToNextStep()
            }}
          />

          <ariaComponents.ButtonGroup align="start" className="mt-4">
            <ariaComponents.Button variant="outline" onPress={goToPreviousStep}>
              {getText('back')}
            </ariaComponents.Button>

            <ariaComponents.Button variant="ghost-fading" onPress={goToNextStep}>
              {getText('skip')}
            </ariaComponents.Button>
          </ariaComponents.ButtonGroup>
        </div>
      )
    },
  },
  {
    title: 'setDefaultUserGroup',
    text: 'setDefaultUserGroupDescription',
    ignore: (context) => {
      const isOrganizationAdmin =
        context.session && 'user' in context.session ?
          context.session.user.isOrganizationAdmin
        : false

      return context.plan === Plan.free || context.plan === Plan.solo || !isOrganizationAdmin
    },
    hideNext: true,
    hidePrevious: true,
    /**
     * Step component
     */
    component: function CreateUserGroupStep({ goToNextStep, goToPreviousStep }) {
      const { getText } = textProvider.useText()
      const remoteBackend = useRemoteBackendStrict()

      const defaultUserGroupMaxLength = 64

      const listUsersQuery = useSuspenseQuery({
        queryKey: ['users'],
        queryFn: () => remoteBackend.listUsers(),
      })

      const createUserGroupMutation = useMutation(
        backendMutationOptions(remoteBackend, 'createUserGroup', {
          onSuccess: async (result) => {
            await Promise.all([
              listUsersQuery.data.map((user) =>
                remoteBackend.changeUserGroup(user.userId, { userGroups: [result.id] }, user.name),
              ),
            ])

            goToNextStep()
          },
        }),
      )

      return (
        <ariaComponents.Form
          schema={(z) => z.object({ groupName: z.string().min(1).max(defaultUserGroupMaxLength) })}
          className="max-w-96"
          onSubmit={({ groupName }) => createUserGroupMutation.mutateAsync([{ name: groupName }])}
        >
          <ariaComponents.Input
            name="groupName"
            autoComplete="off"
            label={getText('groupNameSettingsInput')}
            description={getText('groupNameSettingsInputDescription', defaultUserGroupMaxLength)}
          />

          <ariaComponents.ButtonGroup align="start">
            <ariaComponents.Button variant="outline" onPress={goToPreviousStep}>
              {getText('back')}
            </ariaComponents.Button>

            <ariaComponents.Form.Submit variant="primary">
              {getText('next')}
            </ariaComponents.Form.Submit>
          </ariaComponents.ButtonGroup>

          <ariaComponents.Form.FormError />
        </ariaComponents.Form>
      )
    },
  },
  {
    title: 'allSet',
    text: 'allSetDescription',
  },
]

/**
 * Setup page
 */
export function Setup() {
  const { getText } = textProvider.useText()
  const { session } = useAuth()
  const isFirstRender = useIsFirstRender()

  const [searchParams] = useSearchParams()

  const userPlan = session && 'user' in session ? session.user.plan ?? Plan.free : Plan.free

  const steps = BASE_STEPS
  const isDebug = searchParams.get('__qd-debg__') === 'true'

  const { stepperState, nextStep, previousStep, currentStep } = stepper.useStepperState({
    steps: steps.length,
    onStepChange: (step, direction) => {
      const screen = steps[step]

      if (screen?.ignore != null) {
        if (
          screen.ignore({
            session,
            plan: userPlan,
            goToNextStep: nextStep,
            goToPreviousStep: previousStep,
          })
        ) {
          if (direction === 'forward') {
            nextStep()
          } else {
            previousStep()
          }
        }
      }
    },
  })

  const context = {
    session,
    plan: userPlan,
    goToNextStep: nextStep,
    goToPreviousStep: previousStep,
  }

  const currentScreen = steps.at(currentStep)

  invariant(currentScreen != null, 'Current screen not found')

  if (isFirstRender() && !isDebug) {
    if (session?.type === UserSessionType.full && currentStep === 0) {
      nextStep()
    }
  }

  if (session?.type !== UserSessionType.full && session?.type !== UserSessionType.partial) {
    // eslint-disable-next-line no-restricted-syntax
    return <Navigate to={LOGIN_PATH} />
  }

  const hideNext =
    typeof currentScreen.hideNext === 'function' ?
      currentScreen.hideNext(context)
    : currentScreen.hideNext ?? false
  const canSkip =
    typeof currentScreen.canSkip === 'function' ?
      currentScreen.canSkip(context)
    : currentScreen.canSkip ?? false
  const hidePrevious =
    typeof currentScreen.hidePrevious === 'function' ?
      currentScreen.hidePrevious(context)
    : currentScreen.hidePrevious ?? false

  return (
    <Page>
      <div className="flex flex-1 flex-col overflow-y-auto">
        <div className="mx-auto my-24 w-full max-w-screen-xl px-8 py-6">
          <ariaComponents.Text.Heading level="1" className="mb-4">
            {getText('setupEnso')}
          </ariaComponents.Text.Heading>

          <stepper.Stepper
            state={stepperState}
            renderStep={(stepProps) => {
              const step = steps[stepProps.index]

              invariant(step != null, 'Step not found')

              return (
                <stepper.Stepper.Step
                  {...stepProps}
                  title={getText(step.title)}
                  description={step.description && getText(step.description)}
                  isDisabled={step.ignore?.(context) ?? false}
                >
                  {!stepProps.isLast && <ariaComponents.Separator variant="current" />}
                </stepper.Stepper.Step>
              )
            }}
          >
            {({ isLast, isFirst }) => (
              <div className="flex w-full flex-col gap-6">
                {currentScreen.text && (
                  <ariaComponents.Text>{getText(currentScreen.text)}</ariaComponents.Text>
                )}

                {currentScreen.component && <currentScreen.component {...context} />}

                <ariaComponents.ButtonGroup align="start">
                  {isFirst || hidePrevious ? null : (
                    <ariaComponents.Button variant="outline" onPress={previousStep}>
                      {getText('back')}
                    </ariaComponents.Button>
                  )}

                  {canSkip && (
                    <ariaComponents.Button variant="ghost" onPress={nextStep}>
                      {getText('skip')}
                    </ariaComponents.Button>
                  )}

                  {!hideNext && !isLast && (
                    <ariaComponents.Button variant="primary" onPress={nextStep}>
                      {getText('next')}
                    </ariaComponents.Button>
                  )}

                  {isLast && (
                    <ariaComponents.Button
                      href={DASHBOARD_PATH}
                      variant="primary"
                      size="medium"
                      icon={ArrowRight}
                      iconPosition="end"
                    >
                      {getText('goToDashboard')}
                    </ariaComponents.Button>
                  )}
                </ariaComponents.ButtonGroup>
              </div>
            )}
          </stepper.Stepper>
        </div>
      </div>
    </Page>
  )
}
