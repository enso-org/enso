/** @file A dynamic wizard for creating an arbitrary type of Data Link. */
import * as React from 'react'

import SCHEMA from '#/data/dataLinkSchema.json' assert { type: 'json' }
import * as object from '#/utilities/object'

import Dropdown from '#/components/Dropdown'

// =================
// === lookupDef ===
// =================

const DEFS: Record<string, object> = SCHEMA.$defs

/** Look up a `{ "$ref": "" }` in the root schema. */
function lookupDef(schema: object) {
  const ref = '$ref' in schema && typeof schema.$ref === 'string' ? schema.$ref : null
  const [, name] = ref?.match(/^#[/][$]defs[/](.+)$/) ?? ''
  return name == null ? null : object.asObject(DEFS[name])
}

// ====================
// === isRenderable ===
// ====================

const IS_RENDERABLE = new WeakMap<object, boolean>()

/** Whether the schema will render any elements. */
function isRenderableHelper(schema: object): boolean {
  if ('const' in schema) {
    return false
  } else if ('type' in schema) {
    switch (schema.type) {
      case 'string':
      case 'number':
      case 'integer': {
        return true
      }
      case 'object': {
        const propertiesObject =
          'properties' in schema ? object.asObject(schema.properties) ?? {} : {}
        let count = 0
        for (const child of Object.values(propertiesObject)) {
          const childSchema = object.asObject(child)
          if (childSchema == null) {
            continue
          }
          if (isRenderable(childSchema)) {
            count += 1
          }
        }
        return count > 0
      }
      default: {
        return false
      }
    }
  } else if ('$ref' in schema) {
    const referencedSchema = lookupDef(schema)
    return referencedSchema == null ? false : isRenderable(referencedSchema)
  } else if ('anyOf' in schema || 'allOf' in schema) {
    const children = 'anyOf' in schema ? schema.anyOf : schema.allOf
    if (!Array.isArray(children)) {
      return false
    } else {
      let count = 0
      for (const child of children) {
        const childSchema = object.asObject(child)
        if (childSchema == null) {
          continue
        }
        if (isRenderable(childSchema)) {
          count += 1
        }
      }
      return count > 0
    }
  } else {
    return false
  }
}

/** Whether the schema will render any elements.
 * This function is a memoized version of {@link isRenderableHelper}. */
function isRenderable(schema: object) {
  const cached = IS_RENDERABLE.get(schema)
  if (cached != null) {
    return cached
  } else {
    const renderable = isRenderableHelper(schema)
    IS_RENDERABLE.set(schema, renderable)
    return renderable
  }
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
    const referencedSchema = lookupDef(schema)
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
  dropdownTitle?: string
  schema?: object
  state: unknown
  setState: (dataLink: unknown) => void
}

/** A dynamic wizard for creating an arbitrary type of Data Link. */
export default function DataLinkWizard(props: DataLinkWizardProps) {
  const { dropdownTitle, schema = SCHEMA.$defs.DataLink, state, setState } = props
  const [selectedChildIndex, setSelectedChildIndex] = React.useState<number | null>(null)
  React.useEffect(() => {
    setState(null)
    if ('const' in schema && state !== schema.const) {
      setState(schema.const)
    }
    // `setState` WILL change when the parent schema is an object schema,
    // because in that case `setState` is not memoized.
    // This will result in `state` always becoming `null`.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [selectedChildIndex])
  // NOTE: `enum` schemas omitted for now as they are not yet used.
  if ('const' in schema) {
    // This value cannot change.
    return null
  } else if ('type' in schema) {
    switch (schema.type) {
      case 'string': {
        return (
          <input
            type="text"
            value={typeof state === 'string' ? state : ''}
            size={1}
            className="rounded-full bg-frame-selected w-40 px-2"
            placeholder="Enter text here"
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
            size={1}
            className="rounded-full bg-frame-selected w-40 px-2"
            placeholder="Enter number here"
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
            size={1}
            className="rounded-full bg-frame-selected w-40 px-2"
            placeholder="Enter integer here"
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
        return !isRenderable(schema) ? null : (
          <div className="flex flex-col gap-1 rounded-2xl bg-frame p-2">
            {propertyDefinitions.map(definition => {
              const { key, schema: childSchema } = definition
              return !isRenderable(childSchema) ? null : (
                <div key={key} className="flex flex-wrap items-center">
                  <div className="inline-block w-24">
                    {'title' in childSchema ? String(childSchema.title) : key}
                  </div>
                  <DataLinkWizard
                    schema={childSchema}
                    state={stateAsObject[key]}
                    setState={newValue => {
                      setState({ ...stateAsObject, [key]: newValue })
                    }}
                  />
                </div>
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
    const referencedSchema = lookupDef(schema)
    if (referencedSchema == null) {
      return <></>
    } else {
      return <DataLinkWizard schema={referencedSchema} state={state} setState={setState} />
    }
  } else if ('anyOf' in schema) {
    if (!Array.isArray(schema.anyOf)) {
      return <></>
    } else {
      const childSchemas = schema.anyOf.flatMap(object.singletonObjectOrNull)
      const selectedChildSchema =
        selectedChildIndex == null ? null : childSchemas[selectedChildIndex]
      const isChildRenderable = selectedChildSchema != null && isRenderable(selectedChildSchema)
      if (selectedChildIndex == null) {
        setSelectedChildIndex(0)
      }
      const dropdown = (
        <Dropdown
          items={childSchemas}
          selectedIndex={selectedChildIndex}
          render={childProps => getSchemaName(childProps.item)}
          className="self-start"
          onClick={(_childSchema, index) => {
            setSelectedChildIndex(index)
          }}
        />
      )
      return (
        <div className={`flex flex-col gap-1 ${isChildRenderable ? 'w-full' : ''}`}>
          {dropdownTitle != null ? (
            <div className="flex items-center gap-2">
              {dropdownTitle}
              {dropdown}
            </div>
          ) : (
            dropdown
          )}
          {isChildRenderable && (
            <DataLinkWizard schema={selectedChildSchema} state={state} setState={setState} />
          )}
        </div>
      )
    }
  } else if ('allOf' in schema) {
    if (!Array.isArray(schema.allOf)) {
      return <></>
    } else {
      const childSchemas = schema.allOf.flatMap(object.singletonObjectOrNull)
      return (
        <div className="flex flex-col gap-1">
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
