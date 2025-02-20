/** @file A hidden file input. */
import {
  Form,
  type FieldPath,
  type FieldProps,
  type FieldStateProps,
  type FieldValues,
  type FieldVariantProps,
  type TSchema,
} from '#/components/AriaComponents'
import { Input, type InputProps } from '#/components/aria'

/** Props for {@link HiddenFile}. */
export interface HiddenFileProps<Schema extends TSchema, TFieldName extends FieldPath<Schema, File>>
  extends FieldStateProps<
      Omit<InputProps, 'children' | 'value'> & { value: FieldValues<Schema>[TFieldName] },
      Schema,
      TFieldName,
      File
    >,
    FieldProps,
    FieldVariantProps {
  /** When true, triggers `form.submit()` on input. */
  readonly autoSubmit?: boolean | undefined
  readonly accept?: string | undefined
}

/** A hidden file input. */
export function HiddenFile<Schema extends TSchema, TFieldName extends FieldPath<Schema, File>>(
  props: HiddenFileProps<Schema, TFieldName>,
) {
  const {
    form,
    autoSubmit = false,
    accept,
    // eslint-disable-next-line @typescript-eslint/naming-convention
    defaultValue: _defaultValue,
    // eslint-disable-next-line @typescript-eslint/naming-convention
    disabled: _disabled,
    ...inputProps
  } = props
  const formInstance = Form.useFormContext(form)

  return (
    <Form.Controller
      {...inputProps}
      control={formInstance.control}
      render={({ field }) => (
        <Input
          type="file"
          className="focus-child w-0"
          accept={accept}
          onChange={(event) => {
            field.onChange(event.target.files?.[0])
            if (autoSubmit) {
              void formInstance.submit()
            }
          }}
        />
      )}
    />
  )
}
