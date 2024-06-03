/**
 * @file
 * Types for the Form component.
 */

import type * as reactHookForm from 'react-hook-form'
import type * as z from 'zod'

/**
 * Field Values type.
 */
export type FieldValues<Schema extends TSchema> = [Schema] extends [never]
  ? reactHookForm.FieldValues
  : z.infer<Schema>

/**
 * Schema type
 */
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type TSchema = z.ZodObject<any>

/**
 * Props for the useForm hook.
 */
export interface UseFormProps<Schema extends TSchema, TFieldValues extends FieldValues<Schema>>
  extends Omit<reactHookForm.UseFormProps<TFieldValues>, 'resetOptions' | 'resolver'> {
  readonly schema: Schema
}

/**
 * Return type of the useForm hook.
 */
export type UseFormReturn<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends Record<string, unknown> | undefined = undefined,
> = reactHookForm.UseFormReturn<TFieldValues, unknown, TTransformedValues>

/**
 * Form State type.
 */
export type FormState<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
> = reactHookForm.FormState<TFieldValues>
