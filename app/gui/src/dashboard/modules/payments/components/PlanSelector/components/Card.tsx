/**
 * @file
 *
 * Card component
 */
import * as React from 'react'

import type * as text from 'enso-common/src/text'

import Check from '#/assets/check_mark.svg'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import SvgMask from '#/components/SvgMask'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'

/** Card props */
export interface CardProps extends React.PropsWithChildren, VariantProps<typeof CARD_STYLES> {
  /** Card title */
  readonly title: text.TextId
  /** Card subtitle */
  readonly subtitle: text.TextId
  /** Card features */
  readonly features: string[]
  readonly pricing?: text.TextId
  readonly submitButton?: React.ReactNode
  readonly learnMore?: React.ReactNode
  readonly className?: string
}

export const CARD_STYLES = tv({
  base: 'flex flex-col border-0.5',
  variants: {
    elevated: {
      none: '',
      true: 'shadow-primary/15 shadow',
      small: 'shadow-primary/15 shadow-sm',
      medium: 'shadow-primary/15 shadow-md',
      large: 'shadow-primary/15 shadow-lg',
      xlarge: 'shadow-primary/15 shadow-xl',
      xxlarge: 'shadow-primary/15 shadow-2xl',
      xxxlarge: 'shadow-primary/15 shadow-3xl',
    },
    highlighted: {
      true: 'outline outline-1.5 -outline-offset-1 outline-primary',
      false: 'border-primary/30',
    },
    rounded: {
      none: '',
      small: 'rounded-sm',
      medium: 'rounded-md',
      large: 'rounded-lg',
      xlarge: 'rounded-xl',
      xxlarge: 'rounded-2xl',
      xxxlarge: 'rounded-3xl',
      xxxxlarge: 'rounded-4xl',
    },
    size: {
      medium: { base: 'p-[19.5px]', separator: '-mx-[19.5px]' },
    },
  },
  slots: {
    features: '',
    separator: 'w-auto',
  },
  defaultVariants: {
    elevated: 'none',
    rounded: 'xxxxlarge',
    size: 'medium',
  },
})

/** Card component */
export function Card(props: CardProps) {
  const {
    children,
    features,
    submitButton,
    title,
    subtitle,
    pricing,
    learnMore,
    className,
    elevated,
    rounded,
    highlighted,
  } = props

  const { getText } = textProvider.useText()

  const classes = CARD_STYLES({ elevated, rounded, highlighted })

  return (
    <div className={classes.base({ className })}>
      <ariaComponents.Text.Heading level={2} disableLineHeightCompensation>
        {getText(title)}
      </ariaComponents.Text.Heading>

      <ariaComponents.Text
        elementType="p"
        variant="subtitle"
        weight="medium"
        disableLineHeightCompensation
      >
        {getText(subtitle)}
      </ariaComponents.Text>

      {pricing && (
        <ariaComponents.Text variant="body" weight="bold" disableLineHeightCompensation>
          {getText(pricing)}
        </ariaComponents.Text>
      )}

      {submitButton != null ?
        <div className="my-4">{submitButton}</div>
      : null}

      <ariaComponents.Separator
        variant="primary"
        className={classes.separator()}
        orientation="horizontal"
      />

      {features.length > 0 && (
        <div className="mt-4">
          <ul className="flex flex-col gap-2">
            {features.map((feature, index) => (
              <li key={index} className="flex items-center gap-1">
                <span className="-mb-[1px] flex h-4 w-4 flex-none place-items-center rounded-full bg-green/30">
                  <SvgMask src={Check} className="text-green" />
                </span>

                <ariaComponents.Text variant="body" weight="medium" disableLineHeightCompensation>
                  {feature}
                </ariaComponents.Text>
              </li>
            ))}
          </ul>
        </div>
      )}

      {learnMore != null && <div className="mt-4">{learnMore}</div>}

      {children}
    </div>
  )
}
