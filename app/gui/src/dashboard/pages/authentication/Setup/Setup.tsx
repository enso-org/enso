/** @file Setup page */
import { Button } from '#/components/Button'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import Page from '#/components/Page'
import { Separator } from '#/components/Separator'
import * as stepper from '#/components/Stepper'
import { Text } from '#/components/Text'
import { useIsFirstRender } from '#/hooks/mountHooks'
import { PlanSelector } from '#/modules/payments'
import { Plan } from '#/services/Backend'
import { LOGIN_PATH } from '$/appUtils'
import { UserSessionType } from '$/providers/auth'
import { useAuth, useRouter, useText, useUserSession } from '$/providers/react'
import type * as text from 'enso-common/src/text'
import * as React from 'react'
import invariant from 'tiny-invariant'

/** Step in the setup process */
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

/** Context for the setup process */
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
    /** Setup step for setting username. */
    component: function SetUsernameStep({ session, goToNextStep }) {
      const { setUsername } = useAuth()
      const userSession = useUserSession()
      const { getText } = useText()

      const isUserCreated = userSession?.type === UserSessionType.full
      const defaultName =
        session && 'user' in session ? session.user.name : (userSession?.email ?? '')

      return (
        <Form
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
          onSubmit={async ({ username }) => {
            // If user is already created we shouldn't call `setUsername` if the value has not been
            // changed.
            if (username !== defaultName || !isUserCreated) {
              await setUsername(username)
            }
            goToNextStep()
          }}
        >
          <Input
            name="username"
            label={getText('userNameSettingsInput')}
            placeholder={getText('usernamePlaceholder')}
            description="Minimum 3 characters, maximum 48 characters"
          />

          <Form.Submit variant="primary">{getText('next')}</Form.Submit>

          <Form.FormError />
        </Form>
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
    /** Setup step for choosing plan. */
    component: function ChoosePlanStep({ plan, session }) {
      const isOrganizationAdmin =
        session && 'user' in session ? session.user.isOrganizationAdmin : false

      return <PlanSelector showFreePlan userPlan={plan} isOrganizationAdmin={isOrganizationAdmin} />
    },
  },
]

/** Setup page */
export function Setup() {
  const { getText } = useText()
  const session = useUserSession()
  const isFirstRender = useIsFirstRender()
  const { router, route } = useRouter()

  const userPlan = session && 'user' in session ? session.user.plan : Plan.free

  const steps = BASE_STEPS
  const isDebug = route.query['__qd-debg__'] === 'true'

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
    void router.push(LOGIN_PATH)
    return
  }

  const hideNext =
    typeof currentScreen.hideNext === 'function' ?
      currentScreen.hideNext(context)
    : (currentScreen.hideNext ?? false)
  const canSkip =
    typeof currentScreen.canSkip === 'function' ?
      currentScreen.canSkip(context)
    : (currentScreen.canSkip ?? false)
  const hidePrevious =
    typeof currentScreen.hidePrevious === 'function' ?
      currentScreen.hidePrevious(context)
    : (currentScreen.hidePrevious ?? false)

  return (
    <Page>
      <div className="flex flex-1 flex-col overflow-y-auto">
        <div className="mx-auto my-24 w-full max-w-screen-xl px-8 py-6">
          <Text.Heading level="1" className="mb-4">
            {getText('setupEnso')}
          </Text.Heading>

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
                  {!stepProps.isLast && <Separator variant="current" />}
                </stepper.Stepper.Step>
              )
            }}
          >
            {({ isLast, isFirst }) => (
              <div className="flex w-full flex-col gap-6">
                {currentScreen.text && <Text>{getText(currentScreen.text)}</Text>}

                {currentScreen.component && <currentScreen.component {...context} />}

                <Button.Group align="start">
                  {isFirst || hidePrevious ? null : (
                    <Button variant="outline" onPress={previousStep}>
                      {getText('back')}
                    </Button>
                  )}

                  {canSkip && (
                    <Button variant="ghost" onPress={nextStep}>
                      {getText('skip')}
                    </Button>
                  )}

                  {!hideNext && !isLast && (
                    <Button variant="primary" onPress={nextStep}>
                      {getText('next')}
                    </Button>
                  )}
                </Button.Group>
              </div>
            )}
          </stepper.Stepper>
        </div>
      </div>
    </Page>
  )
}
