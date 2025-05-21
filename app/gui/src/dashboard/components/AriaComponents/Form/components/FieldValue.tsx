/** @file Component that passes the value of a field to its children. */
import { memo, useDeferredValue, type ReactNode } from 'react'
import { useWatch } from 'react-hook-form'
import { useFormContext } from './hooks'
import type { FieldPath, FieldValues, FormInstanceValidated, TSchema } from './types'

/**
 * Props for the {@link FieldValue} component.
 */
export interface FieldValueProps<
  Schema extends TSchema,
  FieldName extends FieldPath<Schema, Constraint>,
  Constraint,
> {
  readonly form?: FormInstanceValidated<Schema>
  readonly name: FieldName
  readonly children: (value: FieldValues<Schema>[FieldName]) => ReactNode
  readonly disabled?: boolean
}

/**
 * Component that subscribes to the value of a field.
 */
export function FieldValue<
  Schema extends TSchema,
  FieldName extends FieldPath<Schema, Constraint>,
  Constraint,
>(props: FieldValueProps<Schema, FieldName, Constraint>) {
  const { form, name, children, disabled = false } = props

  const formInstance = useFormContext(form)
  const value = useWatch({ control: formInstance.control, name, disabled })

  // We use deferred value here to rate limit the re-renders of the children.
  // This is useful when the children are expensive to render, such as a component tree.
  const deferredValue = useDeferredValue(value)

  return <MemoChildren children={children} value={deferredValue} />
}

// Wrap the children to make the deferredValue to work
// see: https://react.dev/reference/react/useDeferredValue#deferring-re-rendering-for-a-part-of-the-ui
const MemoChildren = memo(function MemoChildren<T>(props: {
  children: (value: T) => ReactNode
  value: T
}) {
  return props.children(props.value)
})
