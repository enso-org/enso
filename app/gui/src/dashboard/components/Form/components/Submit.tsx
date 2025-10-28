/**
 * @file
 *
 * Submit button for forms.
 * Manages the form state and displays a loading spinner when the form is submitting.
 */
import { Button, type ButtonProps } from '#/components/Button'
import { useText } from '$/providers/react'
import type { JSX } from 'react'
import { useFormContext } from './FormProvider'
import type { FieldPath, FieldValues, FormInstance, TSchema } from './types'

/** Additional props for the Submit component. */
interface SubmitButtonBaseProps<
  IconType extends string,
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint,
> {
  readonly variant?: ButtonProps<IconType>['variant']
  readonly value?: FieldValues<Schema>[TFieldName]
  readonly name?: TFieldName
  /**
   * Connects the submit button to a form.
   * If not provided, the button will use the nearest form context.
   *
   * This field is helpful when you need to use the submit button outside of the form.
   */
  // We do not need to know the form fields.
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  readonly form?: FormInstance<any>
}

/** Props for the Submit component. */
export type SubmitProps<
  IconType extends string,
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint,
> = Omit<ButtonProps<IconType>, 'formnovalidate' | 'href' | 'variant'> &
  SubmitButtonBaseProps<IconType, Schema, TFieldName, Constraint>

/**
 * Submit button for forms.
 *
 * Manages the form state and displays a loading spinner when the form is submitting.
 */
export function Submit<
  IconType extends string,
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint,
>(props: SubmitProps<IconType, Schema, TFieldName, Constraint>): JSX.Element {
  const { getText } = useText()

  const {
    size = 'medium',
    loading = false,
    children = getText('submit'),
    variant = 'submit',
    testId,
    onPress,
    value,
    name,
    ...buttonProps
  } = props

  const form = useFormContext(props.form)
  const { formState } = form

  return (
    <Button
      type="submit"
      variant={variant}
      size={size}
      isLoading={loading || formState.isSubmitting}
      onPress={(event) => {
        if (value != null && name != null) {
          form.setValue(name, value)
        }

        return onPress?.(event)
      }}
      testId={testId}
      /* This is safe because we are passing all props to the button */
      /* eslint-disable-next-line @typescript-eslint/no-explicit-any,no-restricted-syntax */
      {...(buttonProps as any)}
    >
      {children}
    </Button>
  )
}
