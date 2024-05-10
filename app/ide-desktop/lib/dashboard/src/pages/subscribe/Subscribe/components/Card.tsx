/**
 * @file
 *
 * Card component
 */
import * as React from 'react'

import * as tw from 'tailwind-merge'

import Check from 'enso-assets/check_mark.svg'

import type * as text from '#/text'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import SvgMask from '#/components/SvgMask'

/**
 * Card props
 */
export interface CardProps extends React.PropsWithChildren {
  /**
   * Card title
   */
  readonly title: text.TextId
  /**
   * Card subtitle
   */
  readonly subtitle: text.TextId
  /**
   * Card features
   */
  readonly features: string[]
  readonly pricing?: text.TextId
  readonly submitButton?: React.ReactNode
  readonly learnMore?: React.ReactNode
  readonly className?: string
}

/**
 * Card component
 */
export function Card(props: CardProps) {
  const { children, features, submitButton, title, subtitle, pricing, learnMore, className } = props

  const { getText } = textProvider.useText()

  return (
    <div className={tw.twMerge('rounded-xl border border-primary/10 p-4', className)}>
      <aria.Heading className="text-3xl leading-10" level={2}>
        {getText(title)}
      </aria.Heading>
      <aria.Text elementType="p" className="text-[16px] leading-6">
        {getText(subtitle)}
      </aria.Text>

      {pricing && <aria.Text className="mt-1 flex text-lg">{getText(pricing)}</aria.Text>}

      {submitButton != null ? <div className="my-4">{submitButton}</div> : null}

      <div className="-mx-4 w-auto border-[0.5px] border-primary/10" />

      {features.length > 0 && (
        <div className="mt-4 text-sm">
          <ul className="flex flex-col gap-2">
            {features.map((feature, index) => (
              <li key={index} className="flex gap-1">
                <span className="mt-0.5 flex h-4 flex-none place-items-center rounded-full bg-green/30">
                  <SvgMask src={Check} className="text-green" />
                </span>
                {feature}
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
