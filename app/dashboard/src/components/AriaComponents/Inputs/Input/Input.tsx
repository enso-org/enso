/**
 * @file
 *
 * Basic input component. Input component is a component that is used to get user input in a text field.
 */
import {
  useRef,
  type CSSProperties,
  type ForwardedRef,
  type ReactElement,
  type ReactNode,
  type Ref,
} from 'react'

import * as aria from '#/components/aria'
import {
  Form,
  Text,
  type FieldComponentProps,
  type FieldPath,
  type FieldProps,
  type FieldStateProps,
  type FieldVariantProps,
  type TestIdProps,
  type TSchema,
} from '#/components/AriaComponents'
import SvgMask from '#/components/SvgMask'
import { useAutoFocus } from '#/hooks/autoFocusHooks'
import { mergeRefs } from '#/utilities/mergeRefs'
import { forwardRef } from '#/utilities/react'
import type { ExtractFunction, VariantProps } from '#/utilities/tailwindVariants'
import { omit } from 'enso-common/src/utilities/data/object'
import { INPUT_STYLES } from '../variants'

/**
 * Props for the Input component.
 */
export interface InputProps<Schema extends TSchema, TFieldName extends FieldPath<Schema>>
  extends FieldStateProps<Omit<aria.InputProps, 'children' | 'size'>, Schema, TFieldName>,
    FieldProps,
    FieldVariantProps,
    Omit<VariantProps<typeof INPUT_STYLES>, 'disabled' | 'invalid'>,
    TestIdProps {
  readonly className?: string
  readonly style?: CSSProperties
  readonly inputRef?: Ref<HTMLInputElement>
  readonly addonStart?: ReactNode
  readonly addonEnd?: ReactNode
  readonly placeholder?: string | undefined
  /** The icon to display in the input. */
  readonly icon?: ReactElement | string | null
  readonly variants?: ExtractFunction<typeof INPUT_STYLES> | undefined
  readonly fieldVariants?: FieldComponentProps<Schema>['variants']
}

/**
 * Basic input component. Input component is a component that is used to get user input in a text field.
 */
export const Input = forwardRef(function Input<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema>,
>(props: InputProps<Schema, TFieldName>, ref: ForwardedRef<HTMLFieldSetElement>) {
  const {
    name,
    description,
    inputRef,
    addonStart,
    addonEnd,
    size,
    rounded,
    icon,
    type = 'text',
    variant,
    variants = INPUT_STYLES,
    fieldVariants,
    form,
    autoFocus = false,
    ...inputProps
  } = props

  const testId = props.testId ?? props['data-testid']

  const privateInputRef = useRef<HTMLInputElement>(null)

  const { fieldProps, formInstance } = Form.useFieldRegister<
    Omit<aria.InputProps, 'children' | 'size'>,
    Schema,
    TFieldName
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
        // eslint-disable-next-line @typescript-eslint/no-unsafe-return
        return value
      }
    },
  })

  const classes = variants({
    variant,
    size,
    rounded,
    invalid: fieldProps.isInvalid,
    readOnly: inputProps.readOnly,
    disabled: fieldProps.disabled || formInstance.formState.isSubmitting,
  })

  useAutoFocus({ ref: privateInputRef, disabled: !autoFocus })

  return (
    <Form.Field
      {...aria.mergeProps<FieldComponentProps<Schema>>()(inputProps, omit(fieldProps), {
        isHidden: props.hidden,
        fullWidth: true,
        variants: fieldVariants,
        form: formInstance,
      })}
      ref={ref}
      name={props.name}
      data-testid={testId}
    >
      <div
        className={classes.base()}
        onClick={() => privateInputRef.current?.focus({ preventScroll: true })}
      >
        <div className={classes.content()}>
          {addonStart != null && <div className={classes.addonStart()}>{addonStart}</div>}
          {icon != null &&
            (typeof icon === 'string' ? <SvgMask src={icon} className={classes.icon()} /> : icon)}

          <div className={classes.inputContainer()}>
            <aria.Input
              {...aria.mergeProps<aria.InputProps>()(
                inputProps,
                { className: classes.textArea(), type, name },
                omit(fieldProps, 'isInvalid', 'isRequired', 'isDisabled', 'invalid'),
              )}
              ref={mergeRefs(inputRef, privateInputRef, fieldProps.ref)}
            />
          </div>

          {addonEnd != null && <div className={classes.addonEnd()}>{addonEnd}</div>}
        </div>

        {description != null && (
          <Text slot="description" className={classes.description()}>
            {description}
          </Text>
        )}
      </div>
    </Form.Field>
  )
})
