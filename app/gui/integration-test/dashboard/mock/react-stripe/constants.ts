/** @file Mock for `@stripe/react-stripe-js` */
import type { Stripe, StripeElements } from '@stripe/stripe-js'
import { createContext } from 'react'

/** */
export interface ElementsContextValue {
  elements: StripeElements | null
  stripe: Stripe | null
}

export const ElementsContext = createContext<ElementsContextValue>(null!)
