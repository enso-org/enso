/**
 * @file
 */
import { mergeProps } from '#/components/aria'
import { mergeRefs } from '#/utilities/mergeRefs'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import { omit } from 'enso-common/src/utilities/data/object'
import type { OTPInputProps } from 'input-otp'
import { OTPInput, type SlotProps as OTPInputSlotProps } from 'input-otp'
import type { ForwardedRef, Ref } from 'react'
import { forwardRef, useRef } from 'react'
import type {
  FieldPath,
  FieldProps,
  FieldStateProps,
  FieldValues,
  FieldVariantProps,
  TSchema,
} from '../../Form'
import { Form } from '../../Form'
import { TEXT_STYLE } from '../../Text'
import type { TestIdProps } from '../../types'

/**
 * Props for an {@link OtpInput}.
 */
export interface OtpInputProps<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
> extends FieldStateProps<
      Omit<OTPInputProps, 'children' | 'render'>,
      Schema,
      TFieldValues,
      TFieldName,
      TTransformedValues
    >,
    FieldProps,
    FieldVariantProps,
    Omit<VariantProps<typeof STYLES>, 'disabled' | 'invalid'>,
    TestIdProps {
  readonly inputRef?: Ref<HTMLInputElement>
  readonly maxLength: number
  readonly className?: string
}

const STYLES = tv({
  base: 'group flex overflow-hidden p-1 w-[calc(100%+8px)] -m-1 flex-1',
  slots: {
    slotsContainer: 'flex items-center flex-1 w-full',
  },
})

const SLOT_STYLES = tv({
  base: [
    'flex-1 h-10 min-w-4 flex items-center justify-center',
    'border-primary border-y border-r first:border-l first:border-xl first:rounded-l-xl last:rounded-r-xl',
    'outline outline-1 outline-transparent -outline-offset-2',
    'transition-[outline-offset] duration-200',
  ],
  variants: {
    isActive: { true: 'relative -outline-offset-1 outline-2 outline-primary' },
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

/**
 * Accessible one-time password component with copy paste functionality.
 */
export const OtpInput = forwardRef(function OtpInput<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
>(
  props: OtpInputProps<Schema, TFieldValues, TFieldName, TTransformedValues>,
  ref: ForwardedRef<HTMLFieldSetElement>,
) {
  const {
    maxLength,
    variants = STYLES,
    className,
    name,
    fieldVariants,
    isDisabled = false,
    form,
    defaultValue,
    inputRef,
    ...inputProps
  } = props

  const innerOtpInputRef = useRef<HTMLInputElement>(null)
  const classes = variants({ className })

  const { formInstance, field } = Form.useField({ name, isDisabled, form, defaultValue })
  const fieldProps = formInstance.register(name)

  return (
    <Form.Field
      data-testid={props['data-testid']}
      form={formInstance}
      name={name}
      fullWidth
      isHidden={props.hidden}
      label={props.label}
      aria-label={props['aria-label']}
      aria-labelledby={props['aria-labelledby']}
      aria-describedby={props['aria-describedby']}
      isRequired={fieldProps.isRequired}
      isInvalid={fieldProps.isInvalid}
      aria-details={props['aria-details']}
      ref={ref}
      style={props.style}
      className={props.className}
      variants={fieldVariants}
      description={props.description}
    >
      <OTPInput
        {...mergeProps<OTPInputProps>()(
          inputProps,
          omit(fieldProps, 'required', 'disabled'),
          field,
          {
            name,
            maxLength,
            isDisabled,
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
          },
        )}
        ref={mergeRefs(fieldProps.ref, inputRef, innerOtpInputRef)}
        render={({ slots }) => (
          <div role="presentation" className={classes.slotsContainer()}>
            {slots.map((slot, idx) => (
              <Slot isInvalid={fieldProps.isInvalid} key={idx} {...slot} />
            ))}
          </div>
        )}
      />
    </Form.Field>
  )
})

/**
 * Props for a single {@link Slot}.
 */
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
