import * as React from 'react'

import * as tw from 'tailwind-merge'

import Check from 'enso-assets/check_mark.svg'

import SvgMask from '#/components/SvgMask'

/**
 *
 */
export interface PaywallBulletPointsProps {
  readonly bulletPoints: string[]
  readonly className?: string
}

/**
 *
 */
export function PaywallBulletPoints(props: PaywallBulletPointsProps) {
  const { bulletPoints, className } = props

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
