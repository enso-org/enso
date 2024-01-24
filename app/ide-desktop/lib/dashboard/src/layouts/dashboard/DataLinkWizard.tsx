/** @file A dynamic wizard for creating an arbitrary type of data link. */
import * as React from 'react'

import SCHEMA from '#/data/dataLinkSchema.json' assert { type: 'json' }
import * as object from '#/utilities/object'

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
    const defs: Record<string, object> = SCHEMA.$defs
    const referencedSchema = object.asObject(
      typeof schema.$ref !== 'string' ? null : defs[schema.$ref]
    )
    return referencedSchema == null ? '(unknown)' : getSchemaName(referencedSchema)
  } else if ('anyOf' in schema) {
    const members = Array.isArray(schema.anyOf) ? schema.anyOf : []
    return members.flatMap(object.singletonObjectOrNull).map(getSchemaName).join('|') || '(unknown)'
  } else if ('allOf' in schema) {
    const members = Array.isArray(schema.allOf) ? schema.allOf : []
    return members.flatMap(object.singletonObjectOrNull).join('&') || '(unknown)'
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

// ======================
// === DataLinkWizard ===
// ======================

/** Props for a {@link DataLinkWizard}. */
export interface DataLinkWizardProps {
  schema?: object
  state: unknown
  setState: (dataLink: unknown) => void
}

/** A dynamic wizard for creating an arbitrary type of data link. */
export default function DataLinkWizard(props: DataLinkWizardProps) {
  const { schema = SCHEMA.$defs.DataLink, state, setState } = props
  if ('type' in schema) {
    switch (schema.type) {
      case 'string': {
        return (
          <input
            type="text"
            value={typeof state === 'string' ? state : ''}
            onChange={event => {
              const value: string = event.currentTarget.value
              setState(value)
            }}
          />
        )
      }
      case 'number': {
        return (
          <input
            type="number"
            value={typeof state === 'number' ? state : ''}
            onChange={event => {
              const value: number = event.currentTarget.valueAsNumber
              setState(Number.isNaN(value) ? 0 : value)
            }}
          />
        )
      }
      case 'integer': {
        return (
          <input
            type="number"
            value={typeof state === 'number' ? state : ''}
            onChange={event => {
              const value: number = event.currentTarget.valueAsNumber
              setState(Number.isNaN(value) ? 0 : Math.floor(value))
            }}
          />
        )
      }
      case 'object': {
        const propertiesObject =
          'properties' in schema ? object.asObject(schema.properties) ?? {} : {}
        const propertyDefinitions = Object.entries(propertiesObject).flatMap(
          (kv: [string, unknown]) => {
            const [k, v] = kv
            return object
              .singletonObjectOrNull(v)
              .map(childSchema => ({ key: k, schema: childSchema }))
          }
        )
        const stateAsObject: Record<string, unknown> =
          // This is SAFE, as `state` is an untyped object.
          // eslint-disable-next-line no-restricted-syntax
          (object.asObject(state) as Record<string, unknown> | null) ??
          Object.fromEntries(propertyDefinitions.map(definition => [definition.key, null]))
        return (
          <div className="flex flex-col rounded-2xl bg-frame">
            {propertyDefinitions.map(definition => {
              const { key, schema: childSchema } = definition
              return (
                <DataLinkWizard
                  key={key}
                  schema={childSchema}
                  state={stateAsObject[key]}
                  setState={newValue => {
                    setState({ ...stateAsObject, [key]: newValue })
                  }}
                />
              )
            })}
          </div>
        )
      }
      default: {
        // This is a type we don't care about.
        return <></>
      }
    }
  } else if ('$ref' in schema) {
    const defs: Record<string, object> = SCHEMA.$defs
    const referencedSchema = typeof schema.$ref !== 'string' ? null : defs[schema.$ref]
    if (typeof referencedSchema !== 'object' || referencedSchema == null) {
      return <></>
    } else {
      return <DataLinkWizard schema={SCHEMA} state={state} setState={setState} />
    }
  } else if ('anyOf' in schema) {
    if (!Array.isArray(schema.anyOf)) {
      return <></>
    } else {
      // TODO:
      const childSchemas = schema.anyOf.flatMap(object.singletonObjectOrNull)
      return (
        <div className="flex flex-col rounded-2xl bg-frame">
          {childSchemas.map(childSchema => (
            <>{JSON.stringify(childSchema)}</>
          ))}
        </div>
      )
    }
  } else if ('allOf' in schema) {
    if (!Array.isArray(schema.allOf)) {
      return <></>
    } else {
      // TODO:
      const childSchemas = schema.allOf.flatMap(object.singletonObjectOrNull)
      return (
        <div className="flex flex-col gap-2.5">
          {childSchemas.map((childSchema, i) => (
            <DataLinkWizard key={i} schema={childSchema} state={state} setState={setState} />
          ))}
        </div>
      )
    }
  } else {
    return <></>
  }
}
