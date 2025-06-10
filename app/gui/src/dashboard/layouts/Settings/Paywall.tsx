/**
 * @file
 *
 * This file contains a higher-order component that wraps a component in a paywall.
 * The paywall is shown if the user's plan does not include the feature.
 * The feature is determined by the `isFeatureUnderPaywall` hook.
 */
import { PaywallScreen } from '#/components/Paywall'
import type { PaywallFeatureName } from '#/hooks/billing'
import { tv } from '#/utilities/tailwindVariants'

/** Props for a {@link SettingsPaywall}. */
export interface SettingsPaywallProps {
  readonly feature: PaywallFeatureName
  readonly className?: string | undefined
  readonly onInteracted?: () => void
}

const PAYWALL_LAYOUT_STYLES = tv({ base: 'mt-1' })

/** A layout that shows a paywall for a feature. */
export default function SettingsPaywall(props: SettingsPaywallProps) {
  const { feature, className, onInteracted } = props

  return (
    <div
      className={PAYWALL_LAYOUT_STYLES({ className })}
      onMouseDown={onInteracted}
      onPointerDown={onInteracted}
      onFocus={onInteracted}
    >
      <PaywallScreen feature={feature} />
    </div>
  )
}
