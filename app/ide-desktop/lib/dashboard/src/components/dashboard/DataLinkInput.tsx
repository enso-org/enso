/** @file A dynamic wizard for creating an arbitrary type of Data Link. */
import * as React from 'react'

import Ajv from 'ajv/dist/2020'

import SCHEMA from '#/data/dataLinkSchema.json' assert { type: 'json' }

import type * as jsonSchemaInput from '#/components/JSONSchemaInput'
import JSONSchemaInput from '#/components/JSONSchemaInput'

import * as error from '#/utilities/error'
import * as jsonSchema from '#/utilities/jsonSchema'
import * as object from '#/utilities/object'

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
// === getSchemaName ===
// =====================

const SCHEMA_NAMES = new WeakMap<object, string>()

/** Return a human-readable name representing a schema. */
function getSchemaNameHelper(schema: object): string {
  if ('title' in schema) {
    return String(schema.title)
  } else if ('type' in schema) {
    return String(schema.type)
  } else if ('$ref' in schema) {
    const referencedSchema = jsonSchema.lookupDef(DEFS, schema)
    return referencedSchema == null ? '(unknown)' : getSchemaName(referencedSchema)
  } else if ('anyOf' in schema) {
    const members = Array.isArray(schema.anyOf) ? schema.anyOf : []
    return (
      members.flatMap(object.singletonObjectOrNull).map(getSchemaName).join(' | ') || '(unknown)'
    )
  } else if ('allOf' in schema) {
    const members = Array.isArray(schema.allOf) ? schema.allOf : []
    return members.flatMap(object.singletonObjectOrNull).join(' & ') || '(unknown)'
  } else {
    return '(unknown)'
  }
}

/** Return a human-readable name representing a schema.
 * This function is a memoized version of {@link getSchemaNameHelper}. */
function getSchemaName(schema: object) {
  const cached = SCHEMA_NAMES.get(schema)
  if (cached != null) {
    return cached
  } else {
    const name = getSchemaNameHelper(schema)
    SCHEMA_NAMES.set(schema, name)
    return name
  }
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
