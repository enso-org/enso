/**
 * @file
 *
 * Features list for a plan, rendered as a list of checkmarks with text
 */
import Check from '#/assets/check_mark.svg'

import { Text } from '#/components/AriaComponents'
import SvgMask from '#/components/SvgMask'

/** Props for the PlanFeatures component */
export interface PlanFeaturesProps {
  readonly features: string[]
}

/** Render a list of features */
export function PlanFeatures(props: PlanFeaturesProps) {
  const { features } = props

  return (
    <ul className="flex flex-col gap-1">
      {features.map((feature, index) => (
        <li key={index} className="flex items-center gap-1">
          <span className="-mb-[1.5px] flex h-4 w-4 flex-none place-items-center rounded-full bg-green/30">
            <SvgMask src={Check} className="text-green" />
          </span>

          <Text variant="body" weight="medium" disableLineHeightCompensation>
            {feature}
          </Text>
        </li>
      ))}
    </ul>
  )
}
