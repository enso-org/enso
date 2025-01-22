/** @file Text input. */
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

/** Props for an {@link Input}. */
export interface InputProps<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint extends number | string = number | string,
> extends FieldStateProps<
      Omit<aria.InputProps, 'children' | 'size'>,
      Schema,
      TFieldName,
      Constraint
    >,
    FieldProps,
    FieldVariantProps,
    Omit<VariantProps<typeof INPUT_STYLES>, 'disabled' | 'invalid'>,
    TestIdProps {
  // readonly className?: string
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

/** Basic input component. Input component is a component that is used to get user input in a text field. */
export const Input = forwardRef(function Input<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint extends number | string = number | string,
>(props: InputProps<Schema, TFieldName, Constraint>, ref: ForwardedRef<HTMLDivElement>) {
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
    form: formRaw,
    autoFocus = false,
    className,
    testId: testIdRaw,
    ...inputProps
  } = props
  const form = Form.useFormContext(formRaw)
  const testId = testIdRaw ?? props['data-testid']
  const privateInputRef = useRef<HTMLInputElement>(null)

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

  useAutoFocus({ ref: privateInputRef, disabled: !autoFocus })

  return (
    <Form.Field
      {...aria.mergeProps<FieldComponentProps<Schema>>()(inputProps, fieldProps, {
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
                  className: (classNameStates) =>
                    classes.textArea({ className: computedClassName(classNameStates) }),
                  type,
                  name,
                  // eslint-disable-next-line @typescript-eslint/naming-convention
                  'aria-invalid': invalid,
                },
                omit(inputProps, 'isInvalid', 'isRequired', 'isDisabled'),
                omit(fieldProps, 'isInvalid', 'isRequired', 'isDisabled', 'invalid'),
              )}
              ref={(el) => {
                mergeRefs(inputRef, privateInputRef, fieldProps.ref)(el)
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
    </Form.Field>
  )
})
