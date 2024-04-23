import * as React from 'react'

import * as text from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

/**
 *
 */
export interface ContactSalesProps {}

/**
 *
 */
export function ContactSales(props: ContactSalesProps) {
  const {} = props
  const { getText } = text.useText()

  return (
    <div className="flex flex-col items-center gap-2 text-balance">
      <aria.Heading level={2} className="text-xl">
        {getText('contactSales')}
      </aria.Heading>
      <aria.Text elementType="p" className="text-center text-sm">
        {getText('contactSalesDescription')}
      </aria.Text>

      <ariaComponents.Button variant="primary" onPress={() => {}} size="large" className="mt-8">
        {getText('ContactSalesButtonLabel')}
      </ariaComponents.Button>
    </div>
  )
}
