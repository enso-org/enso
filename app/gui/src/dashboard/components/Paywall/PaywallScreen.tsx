/** @file A screen that shows a paywall. */
import { Text } from '#/components/Text'
import { usePaywallFeatures, type PaywallFeatureName } from '#/hooks/billing'
import { twMerge } from '#/utilities/tailwindMerge'
import { useText } from '$/providers/react'
import { PaywallBulletPoints, PaywallLock } from './components'
import { UpgradeButton } from './UpgradeButton'

/** Props for a {@link PaywallScreen}. */
export interface PaywallScreenProps {
  readonly feature: PaywallFeatureName
  readonly className?: string
}

/** A screen that shows a paywall. */
export function PaywallScreen(props: PaywallScreenProps) {
  const { feature, className } = props
  const { getText } = useText()

  const { getFeature } = usePaywallFeatures()

  const { bulletPointsTextId, descriptionTextId } = getFeature(feature)

  return (
    <div className={twMerge('flex flex-col items-start', className)}>
      <PaywallLock feature={feature} />

      <Text.Heading level="2">{getText('paywallScreenTitle')}</Text.Heading>

      <Text balance variant="subtitle" className="mt-1 max-w-[720px]">
        {getText(descriptionTextId)}
      </Text>

      <PaywallBulletPoints bulletPointsTextId={bulletPointsTextId} className="my-3" />

      <UpgradeButton feature={feature} className="mt-0.5 min-w-36" />
    </div>
  )
}
