/** @file A list of bullet points for a paywall. */
import Check from '#/assets/check_mark.svg'
import SvgMask from '#/components/SvgMask'
import { Text } from '#/components/Text'
import { twMerge } from '#/utilities/tailwindMerge'
import { useText } from '$/providers/react'
import type { TextId } from 'enso-common/src/text'

/** Props for a {@link PaywallBulletPoints}. */
export interface PaywallBulletPointsProps {
  readonly bulletPointsTextId: TextId
  readonly className?: string
}

/** A component that renders a list of bullet points for a paywall. */
export function PaywallBulletPoints(props: PaywallBulletPointsProps) {
  const { bulletPointsTextId, className } = props

  const { getText } = useText()
  const bulletPoints = getText(bulletPointsTextId)
    .split(';')
    .map((bulletPoint) => bulletPoint.trim())

  if (bulletPoints.length === 0) {
    return null
  } else {
    return (
      <ul className={twMerge('m-0 flex w-full list-inside list-none flex-col gap-1', className)}>
        {bulletPoints.map((bulletPoint) => (
          <li key={bulletPoint} className="flex items-start gap-1.5">
            <div className="m-0 flex">
              <div className="m-0 flex">
                <span className="mt-1 flex aspect-square h-4 flex-none place-items-center justify-center rounded-full bg-green/30">
                  <SvgMask src={Check} className="text-green" />
                </span>
              </div>
            </div>

            <Text className="flex-grow" variant="body">
              {bulletPoint}
            </Text>
          </li>
        ))}
      </ul>
    )
  }
}
