/**
 * @file
 *
 * PaywallButton component
 */

import * as React from 'react'

import PaywallBlocked from 'enso-assets/lock.svg'
import LockIcon from 'enso-assets/lock.svg'

import * as appUtils from '#/appUtils'

import * as billingHooks from '#/hooks/billing'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import SvgMask from '#/components/SvgMask'

import { PaywallBulletPoints } from './PaywallBulletPoints'

/**
 * Props of the PaywallButton component
 */
export type PaywallButtonProps = ariaComponents.ButtonProps & {
  readonly feature: billingHooks.PaywallFeatureName
}

/**
 * PaywallButton component
 */
export function PaywallButton(props: PaywallButtonProps) {
  const { size = 'medium', variant = 'primary', feature, ...buttonProps } = props

  const { getText } = textProvider.useText()

  const { getFeature } = billingHooks.usePaywallFeatures()

  const { bulletPointsTextId, level, name, descriptionTextId } = getFeature(feature)
  const levelLabel = getText(level.label)

  const isEnterprise = level === billingHooks.PAYWALL_LEVELS.enterprise

  return (
    <ariaComponents.DialogTrigger>
      <ariaComponents.Button
        variant={variant}
        size={size}
        rounding="full"
        icon={PaywallBlocked}
        iconPosition="start"
        tooltip={getText('paywallScreenDescription', levelLabel)}
        {...buttonProps}
      />

      <ariaComponents.Dialog type="modal" title={getText(name)}>
        <div className="flex flex-col">
          <div className="mb-2 flex flex-col items-center justify-center">
            <div className="flex w-full items-center gap-1 text-sm font-normal">
              <SvgMask src={LockIcon} role="presentation" className="h-4 w-4" />
              {getText('paywallAvailabilityLevel', levelLabel)}
            </div>
          </div>

          <p className="text-base text-primary/60">{getText(descriptionTextId)}</p>

          <PaywallBulletPoints bulletPointsTextId={bulletPointsTextId} className="my-4" />

          <ariaComponents.Button
            variant="primary"
            size="medium"
            rounding="xlarge"
            className="mt-2"
            href={appUtils.SUBSCRIBE_PATH + '?plan=' + level.name}
          >
            {isEnterprise ? getText('contactSales') : getText('learnMore')}
          </ariaComponents.Button>
        </div>
      </ariaComponents.Dialog>
    </ariaComponents.DialogTrigger>
  )
}
