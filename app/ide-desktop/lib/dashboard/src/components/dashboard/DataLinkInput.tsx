/** @file A dynamic wizard for creating an arbitrary type of Data Link. */
import * as React from 'react'

import Ajv from 'ajv/dist/2020'

import SCHEMA from '#/data/dataLinkSchema.json' assert { type: 'json' }

import type * as jsonSchemaInput from '#/components/JSONSchemaInput'
import JSONSchemaInput from '#/components/JSONSchemaInput'

import * as error from '#/utilities/error'

// =================
// === Constants ===
// =================

const DEFS: Record<string, object> = SCHEMA.$defs
// eslint-disable-next-line @typescript-eslint/naming-convention
const AJV = new Ajv({ formats: { 'enso-secret': true } })
AJV.addSchema(SCHEMA)

// ====================
// === getValidator ===
// ====================

/** Get a known schema using a path.
 * @throws {Error} when there is no schema present at the given path. */
function getValidator(path: string) {
  return error.assert<(value: unknown) => boolean>(() => AJV.getSchema(path))
}

// =====================
// === DataLinkInput ===
// =====================

/** Props for a {@link DataLinkInput}. */
export interface DataLinkInputProps
  extends Omit<jsonSchemaInput.JSONSchemaInputProps, 'defs' | 'getValidator' | 'path' | 'schema'> {}

/** A dynamic wizard for creating an arbitrary type of Data Link. */
export default function DataLinkInput(props: DataLinkInputProps) {
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
