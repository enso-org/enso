/**
 * @file
 */

import * as React from 'react'

import * as twMerge from 'tailwind-merge'

import Edit from 'enso-assets/pen.svg'

import * as text from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

import AddPaymentMethodModal from '#/modals/AddPaymentMethodModal'

/**
 *
 */
export interface PaymentMethodProps {
  readonly className?: string
  readonly currentPaymentMethod: {
    id: string
    type: string
    last4: string
    expMonth: number
    expYear: number
  }
  readonly paymentMethods: {
    id: string
    type: string
    last4: string
    expMonth: number
    expYear: number
  }[]
  readonly onAddPaymentMethod: () => void
  readonly onRemovePaymentMethod: (id: string) => void
  readonly onSetDefaultPaymentMethod: (id: string) => void
  readonly onEditPaymentMethod: (id: string) => void
}

/**
 * PaymentMethod component
 */
export function PaymentMethod(props: PaymentMethodProps) {
  const {
    className,
    currentPaymentMethod,
    paymentMethods,
    onAddPaymentMethod,
    onSetDefaultPaymentMethod,
  } = props
  const { getText } = text.useText()

  const schema = ariaComponents.Form.useFormSchema(z => z.object({ paymentMethod: z.string() }))

  return (
    <div
      className={twMerge.twMerge(
        '-ml-5 -mt-1 table table-auto border-spacing-x-5 border-spacing-y-1',
        className
      )}
    >
      <div className="table-row">
        <div className="table-cell">
          <ariaComponents.Text weight="medium" nowrap>
            {getText('billingPageCurrentPaymentMethod')}
          </ariaComponents.Text>
        </div>
        <div className="table-cell">
          <div className="flex items-center gap-1.5">
            <ariaComponents.Text weight="semibold">
              {getText(
                'billingPagePaymentMethod',
                currentPaymentMethod.type,
                currentPaymentMethod.last4
              )}
            </ariaComponents.Text>

            <ariaComponents.DialogTrigger>
              <ariaComponents.Button
                variant="icon"
                size="small"
                icon={Edit}
                aria-label={getText('billingPageEditPaymentMethod')}
              />

              <ariaComponents.Popover size="medium">
                <ariaComponents.Form
                  schema={schema}
                  defaultValues={{ paymentMethod: currentPaymentMethod.id }}
                  method="dialog"
                  gap="medium"
                  onSubmit={({ paymentMethod }) => {
                    onSetDefaultPaymentMethod(paymentMethod)
                  }}
                >
                  {({ form }) => (
                    <>
                      <div className="flex flex-col">
                        <ariaComponents.Text.Heading
                          level="2"
                          variant="h1"
                          className="flex flex-col"
                          disableLineHeightCompensation
                        >
                          {getText('billingPageSelectPaymentMethod')}
                        </ariaComponents.Text.Heading>

                        <ariaComponents.Text elementType="p" disableLineHeightCompensation>
                          {getText('billingPageSelectPaymentMethodDescription')}
                        </ariaComponents.Text>
                      </div>

                      <div className="flex w-full flex-col items-start gap-0.5">
                        <ariaComponents.Radio.Group
                          form={form}
                          name="paymentMethod"
                          fullWidth
                          label={getText('paymentMethod')}
                        >
                          {paymentMethods.map(paymentMethod => (
                            <ariaComponents.Radio key={paymentMethod.id} value={paymentMethod.id}>
                              <span className="flex w-full justify-between">
                                <span>
                                  {getText(
                                    'billingPagePaymentMethod',
                                    paymentMethod.type,
                                    paymentMethod.last4
                                  )}
                                </span>
                                <span>
                                  {getText(
                                    'billingPageExpires',
                                    paymentMethod.expMonth,
                                    paymentMethod.expYear
                                  )}
                                </span>
                              </span>
                            </ariaComponents.Radio>
                          ))}
                        </ariaComponents.Radio.Group>

                        <ariaComponents.DialogTrigger>
                          <ariaComponents.Button variant="link" size="small" className="mt-1">
                            {getText('billingPageAddPaymentMethod')}
                          </ariaComponents.Button>

                          <AddPaymentMethodModal
                            title={getText('billingPageAddPaymentMethod')}
                            onSubmit={onAddPaymentMethod}
                            submitText={getText('billingPageAddPaymentMethod')}
                          />
                        </ariaComponents.DialogTrigger>
                      </div>

                      <ariaComponents.Form.FormError />

                      <ariaComponents.ButtonGroup direction="row" align="start">
                        <ariaComponents.Form.Submit
                          size="medium"
                          fullWidth
                          onPress={() => {
                            onSetDefaultPaymentMethod(currentPaymentMethod.id)
                          }}
                        >
                          {getText('update')}
                        </ariaComponents.Form.Submit>
                      </ariaComponents.ButtonGroup>
                    </>
                  )}
                </ariaComponents.Form>
              </ariaComponents.Popover>
            </ariaComponents.DialogTrigger>
          </div>
        </div>
      </div>
    </div>
  )
}
