/** @file A dynamic wizard for creating an arbitrary type of Datalink. */
import * as React from 'react'

import SCHEMA from '#/data/datalinkSchema.json' assert { type: 'json' }
import * as datalinkValidator from '#/data/datalinkValidator'

import type * as jsonSchemaInput from '#/components/JSONSchemaInput'
import JSONSchemaInput from '#/components/JSONSchemaInput'

import * as error from '#/utilities/error'

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
