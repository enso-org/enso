/** @file A lock icon with a label indicating the paywall level required to access a feature. */
import LockIcon from '#/assets/lock.svg'
import SvgMask from '#/components/SvgMask'
import { Text } from '#/components/Text'
import { getFeatureConfiguration, type PaywallFeatureName } from '$/composables/paywall'
import { useText } from '$/providers/react'
import * as tw from 'tailwind-merge'

/** Props for a {@link PaywallLock}. */
export interface PaywallLockProps {
  readonly feature: PaywallFeatureName
  readonly className?: string
}

/** A lock icon with a label indicating the paywall level required to access a feature. */
export function PaywallLock(props: PaywallLockProps) {
  const { feature, className } = props
  const { getText } = useText()

  const { level } = getFeatureConfiguration(feature)
  const levelLabel = getText(level.label)

  return (
    <div className={tw.twMerge('flex w-full items-center gap-1', className)}>
      <SvgMask src={LockIcon} className="-mt-0.5 h-4 w-4" />
      <Text variant="subtitle">{getText('paywallAvailabilityLevel', levelLabel)}</Text>
    </div>
  )
}
