/** @file A form with the schema prop pre-filled. */
import type { DateValue } from '#/components/aria'
import {
  Checkbox,
  ComboBox,
  DatePicker,
  Input,
  MultiSelector,
  Password,
  Selector,
  Switch,
  type CheckboxProps,
  type ComboBoxProps,
  type DatePickerProps,
  type InputProps,
  type MultiSelectorProps,
  type PasswordProps,
  type SelectorProps,
  type SwitchProps,
} from '#/components/AriaComponents'
import {
  CheckboxGroup,
  type CheckboxGroupProps,
} from '#/components/AriaComponents/Checkbox/CheckboxGroup'
import { useCallback } from 'react'
import { Form } from '../Form'
import type {
  FieldPath,
  FormPropsWithParentForm,
  TSchema,
  UseFormOptions,
  UseFormReturn,
} from '../types'

/** A function to create a form with a schema. */
export function useFormWithComponents<Schema extends TSchema, SubmitResult = void>(
  optionsOrFormInstance: UseFormOptions<Schema, SubmitResult> | UseFormReturn<Schema>,
) {
  const form = Form.useForm(optionsOrFormInstance)

  const FormWithSchema = useCallback(
    (props: Omit<FormPropsWithParentForm<Schema>, 'form'>) => <Form {...props} form={form} />,
    [form],
  )

  // The below type assertions are SAFE if and only if they are used inside the form
  // returned by this function.
  // eslint-disable-next-line no-restricted-syntax
  const SwitchWithForm = Switch as <TFieldName extends FieldPath<Schema, boolean>>(
    props: Omit<SwitchProps<Schema, TFieldName>, 'form'>,
  ) => JSX.Element

  // eslint-disable-next-line no-restricted-syntax
  const CheckboxGroupWithForm = CheckboxGroup as <
    TFieldName extends FieldPath<Schema, readonly string[]>,
  >(
    props: Omit<CheckboxGroupProps<Schema, TFieldName>, 'form'>,
  ) => JSX.Element

  // eslint-disable-next-line no-restricted-syntax
  const CheckboxWithForm = Checkbox as <TFieldName extends FieldPath<Schema, boolean>>(
    props: Omit<CheckboxProps<Schema, TFieldName>, 'form'>,
  ) => JSX.Element

  // eslint-disable-next-line no-restricted-syntax
  const InputWithForm = Input as <TFieldName extends FieldPath<Schema, number | string>>(
    props: Omit<InputProps<Schema, TFieldName>, 'form'>,
  ) => JSX.Element

  // eslint-disable-next-line no-restricted-syntax
  const SelectorWithForm = Selector as <TFieldName extends FieldPath<Schema, T>, T>(
    props: Omit<SelectorProps<Schema, TFieldName, T>, 'form'>,
  ) => JSX.Element

  // eslint-disable-next-line no-restricted-syntax
  const MultiSelectorWithForm = MultiSelector as <
    TFieldName extends FieldPath<Schema, readonly T[]>,
    T,
  >(
    props: Omit<MultiSelectorProps<Schema, TFieldName, T>, 'form'>,
  ) => JSX.Element

  // eslint-disable-next-line no-restricted-syntax
  const ComboBoxWithForm = ComboBox as <TFieldName extends FieldPath<Schema, string>>(
    props: Omit<ComboBoxProps<Schema, TFieldName>, 'form'>,
  ) => JSX.Element

  // eslint-disable-next-line no-restricted-syntax
  const DatePickerWithForm = DatePicker as <TFieldName extends FieldPath<Schema, DateValue>>(
    props: Omit<DatePickerProps<Schema, TFieldName>, 'form'>,
  ) => JSX.Element

  // eslint-disable-next-line no-restricted-syntax
  const PasswordWithForm = Password as <TFieldName extends FieldPath<Schema, string>>(
    props: Omit<PasswordProps<Schema, TFieldName>, 'form'>,
  ) => JSX.Element

  /* eslint-disable @typescript-eslint/naming-convention */
  return {
    form,
    Form: FormWithSchema,
    Input: InputWithForm,
    Switch: SwitchWithForm,
    Checkbox: CheckboxWithForm,
    CheckboxGroup: CheckboxGroupWithForm,
    Selector: SelectorWithForm,
    MultiSelector: MultiSelectorWithForm,
    ComboBox: ComboBoxWithForm,
    DatePicker: DatePickerWithForm,
    Password: PasswordWithForm,
  }
  /* eslint-enable @typescript-eslint/naming-convention */
}
