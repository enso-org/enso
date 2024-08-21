/** @file A dynamic wizard for creating an arbitrary type of Datalink. */
import * as React from 'react'

import SCHEMA from '#/data/datalinkSchema.json' with { type: 'json' }
import * as datalinkValidator from '#/data/datalinkValidator'

import type * as jsonSchemaInput from '#/components/JSONSchemaInput'
import JSONSchemaInput from '#/components/JSONSchemaInput'

import { FieldError } from '#/components/aria'
import type { FieldValues, FormInstance, TSchema } from '#/components/AriaComponents'
import { useFormContext } from '#/components/AriaComponents/Form/components/useFormContext'
import * as error from '#/utilities/error'
import { Controller, type FieldPath } from 'react-hook-form'

// =================
// === Constants ===
// =================

const DEFS: Record<string, object> = SCHEMA.$defs

// ====================
// === getValidator ===
// ====================

/** Get a known schema using a path.
 * @throws {Error} when there is no schema present at the given path. */
function getValidator(path: string) {
  return error.assert<(value: unknown) => boolean>(() => datalinkValidator.AJV.getSchema(path))
}

// =====================
// === DataLinkInput ===
// =====================

/** Props for a {@link DatalinkInput}. */
export interface DatalinkInputProps
  extends Omit<jsonSchemaInput.JSONSchemaInputProps, 'defs' | 'getValidator' | 'path' | 'schema'> {}

/** A dynamic wizard for creating an arbitrary type of Datalink. */
export default function DatalinkInput(props: DatalinkInputProps) {
  return (
    <JSONSchemaInput
      defs={DEFS}
      schema={SCHEMA.$defs.DataLink}
      path={'#/$defs/DataLink'}
      getValidator={getValidator}
      {...props}
    />
  )
}

/** Props for a {@link DatalinkFormInput}. */
export interface DatalinkFormInputProps<Schema extends TSchema>
  extends Omit<DatalinkInputProps, 'onChange' | 'value'> {
  readonly form?: FormInstance<Schema>
  readonly name: FieldPath<FieldValues<Schema>>
}

/** A dynamic wizard for creating an arbitrary type of Datalink. */
export function DatalinkFormInput<Schema extends TSchema>(props: DatalinkFormInputProps<Schema>) {
  const fallbackForm = useFormContext()
  // eslint-disable-next-line no-restricted-syntax
  const { form = fallbackForm as unknown as FormInstance<Schema>, name, ...inputProps } = props

  return (
    <Controller
      control={form.control}
      name={name}
      render={({ field, fieldState }) => {
        const { value, onChange } = field
        return (
          <>
            <DatalinkInput {...inputProps} value={value} onChange={onChange} />
            <FieldError>{fieldState.error?.message}</FieldError>
          </>
        )
      }}
    />
  )
}
