/** @file */
import { mergeProps } from '#/components/aria'
import { mergeRefs } from '#/utilities/mergeRefs'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import { omit } from 'enso-common/src/utilities/data/object'
import type { OTPInputProps } from 'input-otp'
import { OTPInput as BaseOTPInput, type SlotProps as OTPInputSlotProps } from 'input-otp'
import type { ForwardedRef, Ref } from 'react'
import { forwardRef, useRef } from 'react'
import type {
  FieldComponentProps,
  FieldPath,
  FieldProps,
  FieldStateProps,
  FieldVariantProps,
  TSchema,
} from '../../Form'
import { Form } from '../../Form'
import { Separator } from '../../Separator'
import { TEXT_STYLE } from '../../Text'
import type { TestIdProps } from '../../types'

/** Props for an {@link OTPInput}. */
export interface OtpInputProps<Schema extends TSchema, TFieldName extends FieldPath<Schema>>
  extends FieldStateProps<Omit<OTPInputProps, 'children' | 'render'>, Schema, TFieldName>,
    FieldProps,
    FieldVariantProps,
    Omit<VariantProps<typeof STYLES>, 'disabled' | 'invalid'>,
    TestIdProps {
  readonly inputRef?: Ref<HTMLInputElement>
  readonly maxLength: number
  readonly className?: string
  /**
   * Whether to submit the form when the OTP is filled.
   * @default true
   */
  readonly submitOnComplete?: boolean
  /** Callback when the OTP is filled. */
  readonly onComplete?: () => void
}

const STYLES = tv({
  base: 'group flex overflow-hidden p-1 w-[calc(100%+8px)] -m-1 flex-1',
  slots: {
    slotsContainer: 'flex items-center justify-center flex-1 w-full gap-1',
  },
})

const SLOT_STYLES = tv({
  base: [
    'flex-1 h-10 min-w-8 flex items-center justify-center',
    'border border-primary rounded-xl',
    'outline outline-1 outline-transparent -outline-offset-2',
    'transition-[outline-offset] duration-200',
  ],
  variants: {
    isActive: { true: 'relative outline-offset-0 outline-2 outline-primary' },
    isInvalid: { true: { base: 'border-danger', char: 'text-danger' } },
  },
  slots: {
    char: TEXT_STYLE({
      variant: 'body',
      weight: 'bold',
      color: 'current',
    }),
    fakeCaret:
      'absolute pointer-events-none inset-0 flex items-center justify-center animate-caret-blink before:w-px before:h-5 before:bg-primary',
  },
  compoundVariants: [
    {
      isActive: true,
      isInvalid: true,
      class: { base: 'outline-danger' },
    },
  ],
})

/** Accessible one-time password component with copy paste functionality. */
export const OTPInput = forwardRef(function OTPInput<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema>,
>(props: OtpInputProps<Schema, TFieldName>, ref: ForwardedRef<HTMLFieldSetElement>) {
  const {
    maxLength,
    variants = STYLES,
    className,
    name,
    fieldVariants,
    inputRef,
    submitOnComplete = true,
    onComplete,
    form,
    ...inputProps
  } = props

  const innerOtpInputRef = useRef<HTMLInputElement>(null)
  const classes = variants({ className })

  const { fieldProps, formInstance } = Form.useFieldRegister({
    ...props,
    form,
  })

  return (
    <Form.Field
      {...mergeProps<FieldComponentProps<Schema>>()(inputProps, fieldProps, {
        isHidden: props.hidden,
        fullWidth: true,
        variants: fieldVariants,
        form: formInstance,
      })}
      ref={ref}
      name={props.name}
    >
      <BaseOTPInput
        {...mergeProps<OTPInputProps>()(
          inputProps,
          omit(fieldProps, 'isInvalid', 'isRequired', 'isDisabled', 'invalid'),
          {
            name,
            maxLength,
            noScriptCSSFallback: null,
            containerClassName: classes.base(),
            onClick: () => {
              if (innerOtpInputRef.current) {
                // Check if the input is not already focused
                if (document.activeElement !== innerOtpInputRef.current) {
                  innerOtpInputRef.current.focus()
                }
              }
            },
            onComplete: () => {
              onComplete?.()

              if (submitOnComplete) {
                void formInstance.trigger(name).then(() => formInstance.submit())
              }
            },
          },
        )}
        ref={mergeRefs(fieldProps.ref, inputRef, innerOtpInputRef)}
        render={({ slots }) => {
          const sections = (() => {
            const items = []
            const remainingSlots = slots.length % 3

            const sectionsCount = Math.floor(slots.length / 3) + (remainingSlots > 0 ? 1 : 0)

            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
            if (slots.length < 6) {
              items.push(slots)
            } else {
              for (let i = 0; i < sectionsCount; i++) {
                const section = slots.slice(i * 3, (i + 1) * 3)
                items.push(section)
              }
            }

            return items
          })()

          return (
            <div role="presentation" className="flex w-full items-center gap-2">
              {sections.map((section, idx) => (
                <>
                  <div key={idx} className={classes.slotsContainer()}>
                    {section.map((slot, key) => (
                      <Slot isInvalid={fieldProps.isInvalid} key={key} {...slot} />
                    ))}
                  </div>

                  {idx < sections.length - 1 && (
                    <Separator
                      key={idx + 'separator'}
                      orientation="horizontal"
                      className="w-3"
                      size="medium"
                    />
                  )}
                </>
              ))}
            </div>
          )
        }}
      />
    </Form.Field>
  )
})

/** Props for a single {@link Slot}. */
interface SlotProps extends Omit<OTPInputSlotProps, 'isActive'>, VariantProps<typeof SLOT_STYLES> {}

/**
 * Slot is a component that represents a single char in the OTP input.
 * @internal
 */
function Slot(props: SlotProps) {
  const { char, isActive, hasFakeCaret, variants = SLOT_STYLES, isInvalid } = props
  const classes = variants({ isActive, isInvalid })

  return (
    <div className={classes.base()}>
      {char != null && <div className={classes.char()}>{char}</div>}
      {hasFakeCaret && <div role="presentation" className={classes.fakeCaret()} />}
    </div>
  )
}
