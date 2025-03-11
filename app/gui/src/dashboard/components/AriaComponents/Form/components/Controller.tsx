/**
 * @file
 *
 * Controller component allows to pass the imperative form API to the children.
 */
import type { ReactElement } from 'react'
import * as reactHookForm from 'react-hook-form'
import { useFormContext } from './FormProvider'
import type { FieldPath, FieldValues, FormInstance, TSchema } from './types'

/**
 * Props for the {@link Controller} component.
 */
export type ControllerProps<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint,
> = Omit<reactHookForm.ControllerProps<TFieldName>, 'name' | 'render'> & {
  readonly control?: reactHookForm.Control<FieldValues<Schema>>
  readonly name: TFieldName
  readonly form?: FormInstance<Schema>
} & (
    | LegacyControllerProps<Schema, TFieldName, Constraint>
    | ModernControllerProps<Schema, TFieldName, Constraint>
  )

/**
 * Props for the {@link Controller} component.
 */
interface LegacyControllerProps<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint,
> extends Omit<reactHookForm.ControllerProps<TFieldName>, 'name' | 'render'> {
  readonly children?: never
  /**
   * @deprecated Use {@link children} instead.
   */
  readonly render: (props: ControllerRenderProps<Schema, TFieldName, Constraint>) => ReactElement
}

/**
 * Props for the {@link Controller} component.
 */
interface ModernControllerProps<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint,
> extends Omit<reactHookForm.ControllerProps<TFieldName>, 'name' | 'render'> {
  readonly render?: never
  readonly children: (props: ControllerRenderProps<Schema, TFieldName, Constraint>) => ReactElement
}

/**
 * Render props for the {@link Controller} component.
 */
export interface ControllerRenderProps<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint,
> {
  readonly field: reactHookForm.ControllerRenderProps<TFieldName>
  readonly fieldState: reactHookForm.ControllerFieldState
  readonly formState: reactHookForm.UseFormStateReturn<TFieldName>
  readonly form: FormInstance<Schema>
}

/**
 * Component that passes the imperative form API to the children.
 */
export function Controller<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint,
>(props: ControllerProps<Schema, TFieldName, Constraint>) {
  const { form, children, render, control, ...rest } = props

  const formInstance = useFormContext(form)

  return (
    <reactHookForm.Controller
      {...rest}
      control={control ?? formInstance.control}
      render={(field) => {
        const renderProps = {
          ...field,
          form: formInstance,
        }

        return <>{render?.(renderProps) ?? children?.(renderProps)}</>
      }}
    />
  )
}
