/** @file A button to subscribe to a plan. */
import { getSalesEmail } from '#/appUtils'
import { Button, DialogTrigger, Text } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import { PLAN_TO_UPGRADE_LABEL_ID, TRIAL_DURATION_DAYS } from '../../../constants'
import { PlanSelectorDialog, type PlanSelectorDialogProps } from './PlanSelectorDialog'

/** Props for a {@link SubscribeButton}. */
export interface SubscribeButtonProps
  extends Omit<PlanSelectorDialogProps, 'isTrialing' | 'title'> {
  readonly isOrganizationAdmin?: boolean
  readonly userHasSubscription: boolean
  readonly isCurrent?: boolean
  readonly isDowngrade?: boolean
  readonly isDisabled?: boolean
  readonly canTrial?: boolean
  readonly defaultOpen?: boolean
}

/** A button to subscribe to a plan. */
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
    isOrganizationAdmin = false,
  } = props

  const { getText } = useText()

  const buttonText = (() => {
    if (isDowngrade) {
      return getText('downgrade')
    }
    if (isCurrent) {
      return getText('currentPlan')
    }
    if (userHasSubscription) {
      return getText('upgrade')
    }
    if (canTrial) {
      return getText('trialDescription', TRIAL_DURATION_DAYS)
    }
    return getText('subscribe')
  })()

  const description =
    isDowngrade ?
      <Text transform="none">
        <Button variant="link" href={getSalesEmail() + `?subject=Downgrade%20our%20plan`}>
          {getText('contactSales')}
        </Button>{' '}
        {getText('downgradeInfo')}
      </Text>
    : null

  const variant = (() => {
    if (isCurrent || isDowngrade) {
      return 'outline'
    }
    return 'submit'
  })()

  const disabled = isCurrent || isDowngrade || isDisabled || !isOrganizationAdmin

  return (
    <div className="w-full text-center">
      {isDowngrade && (
        <Text transform="normal" className="my-0.5">
          {description}
        </Text>
      )}

      {!isDowngrade && (
        <DialogTrigger
          {...(disabled ? { defaultOpen: false }
          : defaultOpen == null ? {}
          : { defaultOpen })}
        >
          <Button
            fullWidth
            isDisabled={disabled}
            variant={variant}
            size="medium"
            rounded="full"
            aria-label={getText(PLAN_TO_UPGRADE_LABEL_ID[plan])}
          >
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
      )}
    </div>
  )
}
