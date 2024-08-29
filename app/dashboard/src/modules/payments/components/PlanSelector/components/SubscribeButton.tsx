/**
 * @file
 * The subscribe button component.
 */
import { getSalesEmail } from '#/appUtils'

import { useText } from '#/providers/TextProvider'

import { Button, DialogTrigger, Text } from '#/components/AriaComponents'

import { TRIAL_DURATION_DAYS } from '../../../constants'
import { PlanSelectorDialog, type PlanSelectorDialogProps } from './PlanSelectorDialog'

/**
 * The props for the submit button.
 */
export interface SubscribeButtonProps
  extends Omit<PlanSelectorDialogProps, 'isTrialing' | 'title'> {
  readonly userHasSubscription: boolean
  readonly isCurrent?: boolean
  readonly isDowngrade?: boolean
  readonly isDisabled?: boolean
  readonly canTrial?: boolean
  readonly defaultOpen?: boolean
}

/**
 * Subscribe to a plan button
 */
export function SubscribeButton(props: SubscribeButtonProps) {
  const {
    defaultOpen,
    userHasSubscription,
    isCurrent = false,
    isDowngrade = false,
    isDisabled = false,
    canTrial = false,
    plan,
    onSubmit,
    planName,
    features,
  } = props

  const { getText } = useText()

  const buttonText = (() => {
    if (isDowngrade) {
      // eslint-disable-next-line no-restricted-syntax
      return getText('downgrade')
    }

    if (isCurrent) {
      // eslint-disable-next-line no-restricted-syntax
      return getText('currentPlan')
    }

    if (userHasSubscription) {
      // eslint-disable-next-line no-restricted-syntax
      return getText('upgrade')
    }

    // eslint-disable-next-line no-restricted-syntax
    return canTrial ? getText('trialDescription', TRIAL_DURATION_DAYS) : getText('subscribe')
  })()

  const description = (() => {
    if (isDowngrade) {
      // eslint-disable-next-line no-restricted-syntax
      return (
        <>
          {getText('downgradeInfo')}{' '}
          <Button variant="link" href={getSalesEmail() + `?subject=Downgrade%20our%20plan`}>
            {getText('contactSales')}
          </Button>
        </>
      )
    }

    return null
  })()

  const variant = (() => {
    if (isCurrent) {
      // eslint-disable-next-line no-restricted-syntax
      return 'outline'
    }

    if (isDowngrade) {
      // eslint-disable-next-line no-restricted-syntax
      return 'outline'
    }

    return 'submit'
  })()

  const disabled = isCurrent || isDowngrade || isDisabled

  return (
    <div className="w-full text-center">
      <DialogTrigger
        {...(disabled ? { defaultOpen: false }
        : defaultOpen == null ? {}
        : { defaultOpen })}
      >
        <Button fullWidth isDisabled={disabled} variant={variant} size="medium" rounded="full">
          {buttonText}
        </Button>

        <PlanSelectorDialog
          plan={plan}
          planName={planName}
          features={features}
          onSubmit={onSubmit}
          isTrialing={canTrial}
          title={getText('upgradeTo', getText(plan))}
        />
      </DialogTrigger>

      {isDowngrade && (
        <Text transform="capitalize" className="my-0.5">
          {description}
        </Text>
      )}
    </div>
  )
}
