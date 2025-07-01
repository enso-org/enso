/** @file Text input. */
import * as aria from '#/components/aria'
import {
  Form,
  type FieldComponentProps,
  type FieldPath,
  type FieldProps,
  type FieldStateProps,
  type FieldVariantProps,
  type TSchema,
} from '#/components/Form'
import SvgMask from '#/components/SvgMask'
import { Text } from '#/components/Text'
import type { TestIdProps } from '#/components/types'
import { useAutoFocus } from '#/hooks/autoFocusHooks'
import { mergeRefs } from '#/utilities/mergeRefs'
import type { ExtractFunction, VariantProps } from '#/utilities/tailwindVariants'
import { omit } from 'enso-common/src/utilities/data/object'
import {
  forwardRef,
  useRef,
  type CSSProperties,
  type ForwardedRef,
  type ReactElement,
  type ReactNode,
  type Ref,
} from 'react'
import { INPUT_STYLES } from '../variants'

/** Props for an {@link Input}. */
export interface InputProps<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint extends number | string = number | string,
> extends FieldStateProps<
      Omit<aria.InputProps, 'autoFocus' | 'children' | 'size'>,
      Schema,
      TFieldName,
      Constraint
    >,
    FieldProps,
    FieldVariantProps,
    Omit<VariantProps<typeof INPUT_STYLES>, 'disabled' | 'invalid'>,
    TestIdProps {
  /**
   * If `true`, the input will be focused when the component is mounted.
   * If `select`, the input will be focused and the text will be selected.
   */
  readonly autoFocus?: boolean | 'select' | undefined
  readonly style?: CSSProperties
  readonly inputRef?: Ref<HTMLInputElement>
  readonly addonStart?: ReactNode
  readonly addonEnd?: ReactNode
  readonly placeholder?: string | undefined
  /** The icon to display in the input. */
  readonly icon?: ReactElement | string | null
  readonly variants?: ExtractFunction<typeof INPUT_STYLES> | undefined
  readonly fieldVariants?: FieldComponentProps<Schema>['variants']
  readonly fieldClassName?: string | undefined
}

/** Basic input component. Input component is a component that is used to get user input in a text field. */
export const Input = forwardRef(function Input<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint extends number | string = number | string,
>(props: InputProps<Schema, TFieldName, Constraint>, ref: ForwardedRef<HTMLDivElement>) {
  const {
    name,
    inputRef,
    size,
    rounded,
    type = 'text',
    variant,
    variants = INPUT_STYLES,
    fieldVariants,
    fieldClassName,
    form: formRaw,
    className,
    contextualHelp,
    testId: testIdRaw,
    description,
    ...inputProps
  } = props

  const form = Form.useFormContext(formRaw)
  const testId = testIdRaw ?? props['data-testid']

  const { fieldProps, formInstance } = Form.useFieldRegister<
    Omit<aria.InputProps, 'children' | 'size'>,
    Schema,
    TFieldName,
    Constraint
  >({
    ...props,
    form,
    setValueAs: (value: unknown) => {
      if (typeof value === 'string') {
        if (type === 'number') {
          return Number(value)
        } else if (type === 'date') {
          return new Date(value)
        } else {
          return value
        }
      } else {
        return value
      }
    },
  })

  const invalid = inputProps.isInvalid ?? fieldProps.isInvalid
  const disabled = fieldProps.disabled || formInstance.formState.isSubmitting

  const classes = variants({
    variant,
    size,
    rounded,
    invalid,
    readOnly: inputProps.readOnly,
    disabled,
  })

  const computedClassName = (states: aria.InputRenderProps) => {
    if (typeof className === 'function') {
      return className({
        ...states,
        defaultClassName: classes.textArea(),
      })
    } else {
      return className
    }
  }

  return (
    <Form.Field
      {...aria.mergeProps<FieldComponentProps<Schema>>()(inputProps, fieldProps, {
        isHidden: props.hidden,
        fullWidth: true,
        variants: fieldVariants,
        form: formInstance,
        className: fieldClassName,
      })}
      ref={ref}
      name={name}
      data-testid={testId}
      contextualHelp={contextualHelp}
    >
      <BasicInput
        {...aria.mergeProps<BasicInputProps>()(
          {
            className: (classNameStates) =>
              classes.textArea({ className: computedClassName(classNameStates) }),
            type,
            name,
            isInvalid: invalid,
            isDisabled: disabled,
            variant,
            size,
            rounded,
            variants,
            description,
          },
          omit(inputProps, 'isInvalid', 'isRequired', 'isDisabled'),
          omit(fieldProps, 'isInvalid', 'isRequired', 'isDisabled', 'invalid'),
        )}
        ref={(el) => {
          mergeRefs(inputRef, fieldProps.ref)(el)
        }}
      />
    </Form.Field>
  )
})

/** Props for an {@link BasicInput}. */
export interface BasicInputProps
  extends Omit<aria.InputProps, 'autoFocus' | 'children' | 'size'>,
    Omit<VariantProps<typeof INPUT_STYLES>, 'disabled' | 'invalid'>,
    TestIdProps {
  readonly inputRef?: Ref<HTMLInputElement> | undefined
  readonly description?: ReactNode | undefined
  readonly addonStart?: ReactNode | undefined
  readonly addonEnd?: ReactNode | undefined
  readonly placeholder?: string | undefined
  /** The icon to display in the input. */
  readonly icon?: ReactElement | string | null | undefined
  readonly isInvalid?: boolean | undefined
  readonly isDisabled?: boolean | undefined
  readonly variants?: ExtractFunction<typeof INPUT_STYLES> | undefined
  readonly autoFocus?: boolean | 'select' | undefined
}

/** An input without a {@link Form.Field}. */
export const BasicInput = forwardRef(function BasicInput(
  props: BasicInputProps,
  ref?: Ref<HTMLInputElement>,
) {
  const {
    description,
    addonStart,
    addonEnd,
    icon,
    variant,
    variants = INPUT_STYLES,
    autoFocus = false,
    size,
    rounded,
    isInvalid,
    isDisabled,
    className,
    ...inputProps
  } = props

  const privateInputRef = useRef<HTMLInputElement>(null)

  const classes = variants({
    variant,
    size,
    rounded,
    invalid: isInvalid,
    readOnly: inputProps.readOnly,
    disabled: isDisabled,
  })

  const computedClassName = (states: aria.InputRenderProps) => {
    if (typeof className === 'function') {
      return className({
        ...states,
        defaultClassName: classes.textArea(),
      })
    } else {
      return className
    }
  }

  useAutoFocus({
    ref: privateInputRef,
    disabled: autoFocus === false,
    onFocused: () => {
      if (autoFocus === 'select') {
        privateInputRef.current?.select()
      }
    },
  })

  return (
    <div
      className={classes.base()}
      onClick={() => privateInputRef.current?.focus({ preventScroll: true })}
    >
      <div className={classes.content()}>
        {addonStart != null && (
          <div className={classes.addonStart()} data-testid="addon-start">
            {addonStart}
          </div>
        )}

        {icon != null &&
          (typeof icon === 'string' ? <SvgMask src={icon} className={classes.icon()} /> : icon)}

        <div className={classes.inputContainer()}>
          <aria.Input
            {...aria.mergeProps<aria.InputProps>()(
              {
                className: (states) => classes.textArea({ className: computedClassName(states) }),
                // eslint-disable-next-line @typescript-eslint/naming-convention
                'aria-invalid': isInvalid,
              },
              inputProps,
            )}
            ref={(el) => {
              mergeRefs(ref, privateInputRef)(el)
            }}
            data-testid="input"
          />
        </div>

        {addonEnd != null && (
          <div className={classes.addonEnd()} data-testid="addon-end">
            {addonEnd}
          </div>
        )}
      </div>

      {description != null && (
        <Text slot="description" className={classes.description()} data-testid="description">
          {description}
        </Text>
      )}
    </div>
  )
})
