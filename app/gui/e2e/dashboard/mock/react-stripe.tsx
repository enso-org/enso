/** @file Mock for `@stripe/react-stripe-js` */

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type {
  CardElementProps,
  ElementsConsumer as StripeElementConsumer,
  Elements as StripeElements,
} from '@stripe/react-stripe-js'
import { createContext, useContext, useEffect, useState } from 'react'

/** */
type ElementsContextValue_ = Parameters<Parameters<typeof StripeElementConsumer>[0]['children']>[0]

/** */
interface ElementsContextValue extends ElementsContextValue_ {
  //
}

// eslint-disable-next-line @typescript-eslint/no-non-null-assertion
const ElementsContext = createContext<ElementsContextValue>(null!)

/** Elements provider for Stripe. */
export function Elements(...[props]: Parameters<typeof StripeElements>) {
  const { stripe: stripeRaw, children } = props
  const [stripe, setStripe] = useState(stripeRaw && 'then' in stripeRaw ? null : stripeRaw)
  const [elements] = useState(() => {
    // eslint-disable-next-line no-restricted-syntax
    return {
      getElement: (type) => {
        switch (type) {
          case 'card': {
            return CardElement
          }
          default: {
            // eslint-disable-next-line @typescript-eslint/no-explicit-any, no-restricted-syntax, @typescript-eslint/no-unsafe-return
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
        // eslint-disable-next-line no-restricted-syntax
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
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      on: () => null!,
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      off: () => null!,
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      once: () => null!,
    })
  }, [onReady])

  useEffect(() => {
    // eslint-disable-next-line no-restricted-syntax
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

// eslint-disable-next-line no-restricted-syntax
export const useStripe = () => ({
  confirmCardSetup: () => {},
})

// eslint-disable-next-line no-restricted-syntax
export const useElements = () => ({
  getElement: () => {},
})
