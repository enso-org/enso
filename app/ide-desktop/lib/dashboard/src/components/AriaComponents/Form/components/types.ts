/**
 * @file
 * Types for the Form component.
 */

import type * as reactHookForm from 'react-hook-form'
import type * as z from 'zod'

/**
 * Field Values type.
 */
export type FieldValues = reactHookForm.FieldValues

/**
 * Props for the useForm hook.
 */
export interface UseFormProps<T extends FieldValues>
  extends Omit<reactHookForm.UseFormProps<T>, 'resetOptions' | 'resolver'> {
  // eslint-disable-next-line no-restricted-syntax
  readonly schema?: z.ZodObject<T>
}

/**
 * Return type of the useForm hook.
 */
export type UseFormReturn<
  TFieldValues extends Record<string, unknown>,
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends Record<string, unknown> | undefined = undefined,
> = reactHookForm.UseFormReturn<TFieldValues, unknown, TTransformedValues>

/**
 * Form State type.
 */
export type FormState<TFieldValues extends FieldValues> = reactHookForm.FormState<TFieldValues>
