/**
 * @file
 * Types for the Form component.
 */

import type * as reactHookForm from 'react-hook-form'

/**
 * Form Render Props.
 */
export interface FormStateRenderProps<TFieldValues extends reactHookForm.FieldValues> {
  /**
   * The form state.
   */
  readonly formState: reactHookForm.FormState<TFieldValues>
}

/**
 * Form State type.
 */
export type FormState<TFieldValues extends reactHookForm.FieldValues> =
  reactHookForm.FormState<TFieldValues>
/**
 *
 */
export type UseFormProps<TFieldValues extends reactHookForm.FieldValues> =
  reactHookForm.UseFormProps<TFieldValues>
