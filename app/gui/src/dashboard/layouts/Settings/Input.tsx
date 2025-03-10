/** @file Rendering for an {@link SettingsInputData}. */
import type { FieldPath, TSchema } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import {
  SettingsAriaInput,
  SettingsAriaInputEmail,
  SettingsAriaInputPassword,
  type SettingsAriaInputProps,
} from './AriaInput'
import type { SettingsContext, SettingsInputData, SettingsInputType } from './data'

/** Props for a {@link SettingsInput}. */
export interface SettingsInputProps<T extends Record<keyof T, string>> {
  readonly context: SettingsContext
  readonly data: SettingsInputData<T>
}

/** Rendering for an {@link SettingsInputData}. */
export default function SettingsInput<T extends Record<keyof T, string>>(
  props: SettingsInputProps<T>,
) {
  const { context, data } = props
  const {
    name,
    nameId,
    autoComplete,
    hidden: hiddenRaw,
    editable,
    descriptionId,
    type = 'text',
  } = data
  const { getText } = useText()

  const isEditable = typeof editable === 'function' ? editable(context) : (editable ?? true)
  const hidden = typeof hiddenRaw === 'function' ? hiddenRaw(context) : (hiddenRaw ?? false)

  const Input = INPUT_TYPE_MAP[type]

  return (
    <Input
      readOnly={!isEditable}
      label={getText(nameId)}
      name={name}
      hidden={hidden}
      autoComplete={autoComplete}
      {...(descriptionId != null && {
        description: getText(descriptionId),
      })}
    />
  )
}

const INPUT_TYPE_MAP: Record<
  SettingsInputType,
  React.ComponentType<SettingsAriaInputProps<TSchema, FieldPath<TSchema>>>
> = {
  email: SettingsAriaInputEmail,
  password: SettingsAriaInputPassword,
  text: SettingsAriaInput,
}
