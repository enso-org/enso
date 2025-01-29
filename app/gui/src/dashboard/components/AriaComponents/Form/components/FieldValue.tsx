/**
 * @file
 * Component that passes the value of a field to its children.
 */
import { memo, useDeferredValue, type ReactNode } from 'react'
import { useWatch } from 'react-hook-form'
import { useFormContext } from './FormProvider'
import type { FieldPath, FieldValues, FormInstanceValidated, TSchema } from './types'

/**
 * Props for the {@link FieldValue} component.
 */
export interface FieldValueProps<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint,
> {
  readonly form?: FormInstanceValidated<Schema>
  readonly name: TFieldName
  readonly children: (value: FieldValues<Schema>[TFieldName]) => ReactNode
  readonly disabled?: boolean
}

/**
 * Component that subscribes to the value of a field.
 */
export function FieldValue<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint,
>(props: FieldValueProps<Schema, TFieldName, Constraint>) {
  const { form, name, children, disabled = false } = props

  const formInstance = useFormContext(form)
  const value = useWatch({ control: formInstance.control, name, disabled })

  // We use deferred value here to rate limit the re-renders of the children.
  // This is useful when the children are expensive to render, such as a component tree.
  const deferredValue = useDeferredValue(value)

  return <MemoChildren children={children} value={deferredValue} />
}

// Wrap the childer to make the deferredValue to work
// see: https://react.dev/reference/react/useDeferredValue#deferring-re-rendering-for-a-part-of-the-ui
// eslint-disable-next-line no-restricted-syntax
const MemoChildren = memo(function MemoChildren<T>(props: {
  children: (value: T) => ReactNode
  value: T
}) {
  return props.children(props.value)
}) as unknown as <T>(props: { children: (value: T) => ReactNode; value: T }) => ReactNode
