/** @file A dialog that prompts the user to upgrade to a paid plan. */
import { Dialog, type DialogProps } from '#/components/Dialog'
import { Text } from '#/components/Text'
import { usePaywallFeatures, type PaywallFeatureName } from '#/hooks/billing'
import { useText } from '$/providers/react'
import { PaywallBulletPoints, PaywallLock } from './components'
import { UpgradeButton } from './UpgradeButton'

/** Props for a {@link PaywallDialog}. */
export interface PaywallDialogProps extends DialogProps {
  readonly feature: PaywallFeatureName
}

/** A dialog that prompts the user to upgrade to a paid plan. */
export function PaywallDialog(props: PaywallDialogProps) {
  const { feature, type = 'modal', title, ...dialogProps } = props

  const { getText } = useText()
  const { getFeature } = usePaywallFeatures()

  const { bulletPointsTextId, label, descriptionTextId } = getFeature(feature)

  return (
    <Dialog type={type} title={title ?? getText(label)} {...dialogProps}>
      <div className="flex flex-col">
        <PaywallLock feature={feature} className="mb-2" />

        <Text variant="subtitle">{getText(descriptionTextId)}</Text>

        <PaywallBulletPoints bulletPointsTextId={bulletPointsTextId} className="my-2" />

        <UpgradeButton feature={feature} className="mt-2" size="large" />
      </div>
    </Dialog>
  )
}
