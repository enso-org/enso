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
  type CheckboxGroupCheckboxProps,
  type CheckboxProps,
  type ComboBoxProps,
  type DatePickerProps,
  type InputProps,
  type MultiSelectorProps,
  type PasswordProps,
  type SelectorProps,
  type StandaloneCheckboxProps,
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

/** A function to create a form with a schema */
export function useFormWithComponents<Schema extends TSchema, SubmitResult = void>(
  optionsOrFormInstance: UseFormOptions<Schema, SubmitResult> | UseFormReturn<Schema>,
) {
  const form = Form.useForm(optionsOrFormInstance)

  const FormWithSchema = useCallback(
    (props: Omit<FormPropsWithParentForm<Schema>, 'form'>) => <Form {...props} form={form} />,
    [form],
  )

  const SwitchWithForm = useCallback(
    <TFieldName extends FieldPath<Schema, boolean>>(
      props: Omit<SwitchProps<Schema, TFieldName>, 'form'>,
      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, no-restricted-syntax, @typescript-eslint/no-explicit-any
    ) => <Switch {...props} form={form as any} />,
    [form],
  )

  const CheckboxGroupWithForm = useCallback(
    <TFieldName extends FieldPath<Schema, readonly string[]>>(
      props: Omit<CheckboxGroupProps<Schema, TFieldName>, 'form'>,
      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, no-restricted-syntax, @typescript-eslint/no-explicit-any
    ) => <CheckboxGroup {...props} form={form as any} />,
    [form],
  )

  const CheckboxWithForm = useCallback(
    <TFieldName extends FieldPath<Schema, boolean>>(
      props: Omit<CheckboxProps<Schema, TFieldName>, 'form'>,
    ) =>
      'name' in props ?
        <Checkbox
          // eslint-disable-next-line no-restricted-syntax
          {...(props as Omit<StandaloneCheckboxProps<Schema, TFieldName>, 'form'>)}
          // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, no-restricted-syntax, @typescript-eslint/no-explicit-any
          form={form as any}
        />
        // eslint-disable-next-line no-restricted-syntax
      : <Checkbox {...(props as CheckboxGroupCheckboxProps)} />,
    [form],
  )

  const InputWithForm = useCallback(
    <TFieldName extends FieldPath<Schema, number | string>>(
      props: Omit<InputProps<Schema, TFieldName>, 'form'>,
      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, no-restricted-syntax, @typescript-eslint/no-explicit-any
    ) => <Input {...props} form={form as any} />,
    [form],
  )

  const SelectorWithForm = useCallback(
    <TFieldName extends FieldPath<Schema, T>, T>(
      props: Omit<SelectorProps<Schema, TFieldName, T>, 'form'>,
      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, no-restricted-syntax, @typescript-eslint/no-explicit-any
    ) => <Selector {...props} form={form as any} />,
    [form],
  )

  const MultiSelectorWithForm = useCallback(
    <TFieldName extends FieldPath<Schema, readonly T[]>, T>(
      props: Omit<MultiSelectorProps<Schema, TFieldName, T>, 'form'>,
      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, no-restricted-syntax, @typescript-eslint/no-explicit-any
    ) => <MultiSelector {...props} form={form as any} />,
    [form],
  )

  const ComboBoxWithForm = useCallback(
    <TFieldName extends FieldPath<Schema, string>>(
      props: Omit<ComboBoxProps<Schema, TFieldName>, 'form'>,
      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, no-restricted-syntax, @typescript-eslint/no-explicit-any
    ) => <ComboBox {...props} form={form as any} />,
    [form],
  )

  const DatePickerWithForm = useCallback(
    <TFieldName extends FieldPath<Schema, DateValue>>(
      props: Omit<DatePickerProps<Schema, TFieldName>, 'form'>,
      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, no-restricted-syntax, @typescript-eslint/no-explicit-any
    ) => <DatePicker {...props} form={form as any} />,
    [form],
  )

  const PasswordWithForm = useCallback(
    <TFieldName extends FieldPath<Schema, string>>(
      props: Omit<PasswordProps<Schema, TFieldName>, 'form'>,
      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, no-restricted-syntax, @typescript-eslint/no-explicit-any
    ) => <Password {...props} form={form as any} />,
    [form],
  )

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
