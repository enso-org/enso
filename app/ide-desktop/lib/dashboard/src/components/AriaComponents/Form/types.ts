/**
 * @file
 * Types for the Form component.
 */

import type * as reactHookForm from 'react-hook-form'

import type * as components from './components'

export type * from './components'

/**
 * Form Render Props.
 */
export interface FormStateRenderProps<TFieldValues extends components.FieldValues> {
  /**
   * The form state. Contains the current values of the form fields.
   */
  readonly formState: components.FormState<TFieldValues>
  /**
   * The form register function.
   * Adds a field to the form state.
   */
  readonly register: reactHookForm.UseFormRegister<TFieldValues>
  /**
   * The form unregister function.
   * Removes a field from the form state.
   */
  readonly unregister: reactHookForm.UseFormUnregister<TFieldValues>
}
