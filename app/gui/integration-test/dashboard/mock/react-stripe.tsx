/** @file Mock for `@stripe/react-stripe-js` */

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type {
  CardElementProps,
  ElementsConsumer as StripeElementConsumer,
  Elements as StripeElements,
} from '@stripe/react-stripe-js'
import { createContext, useContext, useEffect, useState } from 'react'

/** */
type ElementsContextValue = Parameters<Parameters<typeof StripeElementConsumer>[0]['children']>[0]

const ElementsContext = createContext<ElementsContextValue>(null!)

/** Elements provider for Stripe. */
export function Elements(...[props]: Parameters<typeof StripeElements>) {
  const { stripe: stripeRaw, children } = props
  const [stripe, setStripe] = useState(stripeRaw && 'then' in stripeRaw ? null : stripeRaw)
  const [elements] = useState(() => {
    return {
      getElement: (type) => {
        switch (type) {
          case 'card': {
            return CardElement
          }
          default: {
            return (<></>) as any
          }
        }
      },
    } satisfies Partial<
      ElementsContextValue['elements']
    > as unknown as ElementsContextValue['elements']
  })

  useEffect(() => {
    let canceled = false
    if (stripeRaw && 'then' in stripeRaw) {
      void stripeRaw.then((awaitedStripe) => {
        if (!canceled) {
          setStripe(awaitedStripe)
        }
      })
    }
    return () => {
      canceled = true
    }
  }, [stripeRaw])

  return (
    stripe && (
      <ElementsContext.Provider
        value={{
          stripe,
          elements,
        }}
      >
        {children}
      </ElementsContext.Provider>
    )
  )
}

/** Elements consumer for Stripe. */
export function ElementsConsumer(...[props]: Parameters<typeof StripeElementConsumer>) {
  return props.children(useContext(ElementsContext))
}

/** Card element for Stripe. */
export function CardElement(props: CardElementProps) {
  const { onReady: onReadyRaw, onChange: onChangeRaw } = props
  const onReady = useEventCallback(onReadyRaw ?? (() => {}))
  const onChange = useEventCallback(onChangeRaw ?? (() => {}))

  useEffect(() => {
    onReady({
      blur: () => {},
      clear: () => {},
      destroy: () => {},
      focus: () => {},
      mount: () => {},
      unmount: () => {},
      update: () => {},
      on: () => null!,
      off: () => null!,
      once: () => null!,
    })
  }, [onReady])

  useEffect(() => {
    onChange({
      elementType: 'card',
      empty: false,
      complete: true,
      error: undefined,
      value: { postalCode: '40001' },
      brand: 'mastercard',
    })
  }, [onChange])

  return <></>
}

export const useStripe = () => ({
  confirmCardSetup: () => {},
})

export const useElements = () => ({
  getElement: () => {},
})
