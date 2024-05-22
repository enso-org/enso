/**
 * @file
 *
 * A component that renders a list of bullet points for a paywall.
 */
import * as React from 'react'

import * as tw from 'tailwind-merge'

import Check from 'enso-assets/check_mark.svg'

import type * as text from '#/text'

import * as textProvider from '#/providers/TextProvider'

import SvgMask from '#/components/SvgMask'

/**
 * Props for a {@link PaywallBulletPoints}.
 */
export interface PaywallBulletPointsProps {
  readonly bulletPointsTextId: text.TextId
  readonly className?: string
}

/**
 * A component that renders a list of bullet points for a paywall.
 */
export function PaywallBulletPoints(props: PaywallBulletPointsProps) {
  const { bulletPointsTextId, className } = props

  const { getText } = textProvider.useText()
  const bulletPoints = getText(bulletPointsTextId)
    .split(';')
    .map(bulletPoint => bulletPoint.trim())

  if (bulletPoints.length === 0) {
    return null
  } else {
    return (
      <ul
        className={tw.twMerge(
          'm-0 flex w-full list-inside list-none flex-col gap-1 text-base',
          className
        )}
      >
        {bulletPoints.map(bulletPoint => (
          <li key={bulletPoint} className="flex items-start gap-1">
            <div className="m-0 flex">
              <div className="m-0 flex">
                <span className="mt-[5px] flex aspect-square h-4 flex-none place-items-center justify-center rounded-full bg-green/30">
                  <SvgMask src={Check} className="text-green" />
                </span>
              </div>
            </div>

            <div className="flex-grow">{bulletPoint}</div>
          </li>
        ))}
      </ul>
    )
  }
}
