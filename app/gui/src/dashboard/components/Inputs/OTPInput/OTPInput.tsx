/** @file */
import { mergeProps } from '#/components/aria'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { mergeRefs } from '#/utilities/mergeRefs'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import type { OTPInputProps } from 'input-otp'
import { OTPInput as BaseOTPInput, type SlotProps as OTPInputSlotProps } from 'input-otp'
import type { ForwardedRef, Ref } from 'react'
import { forwardRef, useRef } from 'react'
import type {
  FieldComponentProps,
  FieldPath,
  FieldProps,
  FieldStateProps,
  FieldValues,
  FieldVariantProps,
  TSchema,
} from '../../Form'
import { filterNonDOMFormProps, Form } from '../../Form'
import { Separator } from '../../Separator'
import { TEXT_STYLE } from '../../Text'
import type { TestIdProps } from '../../types'

/** Props for an {@link OTPInput}. */
export interface OtpInputProps<Schema extends TSchema, TFieldName extends FieldPath<Schema, string>>
  extends FieldStateProps<Omit<OTPInputProps, 'children' | 'render'>, Schema, TFieldName, string>,
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
    char: TEXT_STYLE({ variant: 'body', weight: 'bold', color: 'current' }),
    fakeCaret:
      'absolute pointer-events-none inset-0 flex items-center justify-center animate-caret-blink before:w-px before:h-5 before:bg-primary',
  },
  compoundVariants: [{ isActive: true, isInvalid: true, class: { base: 'outline-danger' } }],
})

/** Accessible one-time password component with copy paste functionality. */
export const OTPInput = forwardRef(function OTPInput<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, string>,
>(props: OtpInputProps<Schema, TFieldName>, ref: ForwardedRef<HTMLDivElement>) {
  const {
    maxLength,
    variants = STYLES,
    className,
    name,
    fieldVariants,
    inputRef,
    submitOnComplete = true,
    onComplete,
    contextualHelp,
    // eslint-disable-next-line @typescript-eslint/naming-convention
    form: _,
    ...inputProps
  } = props

  const innerOtpInputRef = useRef<HTMLInputElement>(null)
  const classes = variants({ className })

  const { fieldProps, formInstance } = Form.useFieldRegister(props)

  const onCompleteInternal = useEventCallback(() => {
    if (innerOtpInputRef.current?.value == null) {
      return
    }

    onComplete?.()

    if (submitOnComplete) {
      formInstance.setValue(
        name,
        // eslint-disable-next-line no-restricted-syntax
        innerOtpInputRef.current.value as FieldValues<Schema>[TFieldName],
        { shouldValidate: true },
      )
      void formInstance.submit()
    }
  })

  const onClickInternal = useEventCallback(() => {
    if (innerOtpInputRef.current) {
      // Check if the input is not already focused
      if (document.activeElement !== innerOtpInputRef.current) {
        innerOtpInputRef.current.focus()
      }
    }
  })

  return (
    <Form.Field
      {...mergeProps<FieldComponentProps<Schema>>()(fieldProps, inputProps, {
        fullWidth: true,
        isHidden: props.hidden,
        variants: fieldVariants,
        form: formInstance,
      })}
      ref={ref}
      name={name}
      contextualHelp={contextualHelp}
    >
      <BaseOTPInput
        {...filterNonDOMFormProps(mergeProps<OTPInputProps>()(fieldProps, inputProps))}
        ref={(el) => {
          mergeRefs(fieldProps.ref, inputRef, innerOtpInputRef)(el)
        }}
        name={name}
        maxLength={maxLength}
        noScriptCSSFallback={null}
        containerClassName={classes.base({ className })}
        onClick={onClickInternal}
        onComplete={onCompleteInternal}
        render={({ slots }) => (
          <OTPInputRenderer
            slots={slots}
            isInvalid={fieldProps.isInvalid}
            slotsContainerClassName={classes.slotsContainer()}
          />
        )}
      />
    </Form.Field>
  )
})

/** Props for an {@link OTPInputRenderer}. */
interface OTPInputRendererProps {
  readonly slots: OTPInputSlotProps[]
  readonly isInvalid: boolean
  readonly slotsContainerClassName: string
}
/**
 * OTPInputRenderer is a component that renders the OTP input.
 * @internal
 */
function OTPInputRenderer(props: OTPInputRendererProps) {
  const { slots, isInvalid, slotsContainerClassName } = props

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
        <Section
          key={idx}
          slotsContainerClassName={slotsContainerClassName}
          isInvalid={isInvalid}
          section={section}
          isLast={idx === sections.length - 1}
        />
      ))}
    </div>
  )
}

/**
 * Props for a {@link Section}.
 * @internal
 */
interface SectionProps {
  readonly slotsContainerClassName: string
  readonly isInvalid: boolean
  readonly section: OTPInputSlotProps[]
  readonly isLast: boolean
}

/**
 * Section is a component that renders a section of the OTP input.
 * @internal
 */
function Section(props: SectionProps) {
  const { slotsContainerClassName, isInvalid, section, isLast } = props

  return (
    <>
      <div className={slotsContainerClassName}>
        {section.map((slot, key) => (
          <Slot isInvalid={isInvalid} key={key} {...slot} />
        ))}
      </div>

      {!isLast && <Separator orientation="horizontal" className="w-3" size="medium" />}
    </>
  )
}

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
