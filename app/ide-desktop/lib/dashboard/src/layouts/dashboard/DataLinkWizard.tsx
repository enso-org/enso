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

const CONSTANT_VALUE = new WeakMap<object, [] | [NonNullable<unknown> | null]>()

/** The value of the schema, if it can only have one possible value. */
function constantValueHelper(schema: object): [] | [NonNullable<unknown> | null] {
  if ('const' in schema) {
    return [schema.const ?? null]
  } else if ('type' in schema) {
    switch (schema.type) {
      case 'string':
      case 'number':
      case 'integer': {
        return []
      }
      case 'object': {
        const propertiesObject =
          'properties' in schema ? object.asObject(schema.properties) ?? {} : {}
        const result: Record<string, unknown> = {}
        for (const [key, child] of Object.entries(propertiesObject)) {
          const childSchema = object.asObject(child)
          if (childSchema == null) {
            continue
          }
          const value = constantValue(childSchema)
          if (value.length === 0) {
            // eslint-disable-next-line no-restricted-syntax
            return []
          } else {
            result[key] = value[0]
          }
        }
        return [result]
      }
      default: {
        return []
      }
    }
  } else if ('$ref' in schema) {
    const referencedSchema = lookupDef(schema)
    return referencedSchema == null ? [] : constantValue(referencedSchema)
  } else if ('anyOf' in schema) {
    if (!Array.isArray(schema.anyOf) || schema.anyOf.length !== 1) {
      return []
    } else {
      const firstMember = object.asObject(schema.anyOf[0])
      return firstMember == null ? [] : constantValue(firstMember)
    }
  } else if ('allOf' in schema) {
    if (!Array.isArray(schema.allOf) || schema.allOf.length === 0) {
      return []
    } else {
      const firstMember = object.asObject(schema.allOf[0])
      const firstValue = firstMember == null ? [] : constantValue(firstMember)
      if (firstValue.length === 0) {
        return []
      } else {
        const result = firstValue[0]
        for (const child of schema.allOf) {
          const childSchema = object.asObject(child)
          if (childSchema == null) {
            continue
          }
          const value = constantValue(childSchema)
          if (value.length === 0) {
            // eslint-disable-next-line no-restricted-syntax
            return []
          } else if (typeof result !== 'object' || result == null) {
            if (result !== value[0]) {
              // eslint-disable-next-line no-restricted-syntax
              return []
            }
          } else {
            if (value[0] == null || typeof result !== typeof value[0]) {
              // eslint-disable-next-line no-restricted-syntax
              return []
            }
            Object.assign(result, value[0])
          }
        }
        return [result]
      }
    }
  } else {
    return [null]
  }
}

/** The value of the schema, if it can only have one possible value.
 * This function is a memoized version of {@link constantValueHelper}. */
function constantValue(schema: object) {
  const cached = CONSTANT_VALUE.get(schema)
  if (cached != null) {
    return cached
  } else {
    const renderable = constantValueHelper(schema)
    CONSTANT_VALUE.set(schema, renderable)
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
  state: NonNullable<unknown> | null
  setState: React.Dispatch<React.SetStateAction<NonNullable<unknown> | null>>
  setIsSubmittable: (isSubmittable: boolean) => void
}

/** A dynamic wizard for creating an arbitrary type of Data Link. */
export default function DataLinkWizard(props: DataLinkWizardProps) {
  const {
    dropdownTitle,
    schema = SCHEMA.$defs.DataLink,
    state: stateRaw,
    setState: setStateRaw,
    setIsSubmittable: setIsSubmittableRaw,
  } = props
  const [selectedChildIndex, setSelectedChildIndex] = React.useState(0)
  const [isSubmittable, setIsSubmittable] = React.useState(false)
  const [childSubmittability, setChildSubmittability] = React.useState<boolean[] | null>(null)
  const [initializing, setInitializing] = React.useState(true)
  const [state, setState] = React.useState(stateRaw)

  React.useEffect(() => {
    if (childSubmittability != null) {
      setIsSubmittable(childSubmittability.every(submittable => submittable))
    }
  }, [childSubmittability])

  React.useEffect(() => {
    setIsSubmittableRaw(isSubmittable)
    // `setIsSubmittableRaw` is a callback, not a dependency.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [isSubmittable])

  React.useEffect(() => {
    setStateRaw(state)
    // `setStateRaw` is a callback, not a dependency.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [state])

  React.useEffect(() => {
    setInitializing(false)
  }, [])

  // NOTE: `enum` schemas omitted for now as they are not yet used.
  if ('const' in schema) {
    if (initializing) {
      setIsSubmittable(true)
      setState(schema.const ?? null)
    }
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
            className="rounded-full w-40 px-2 bg-transparent border border-black/10 leading-170 h-6 py-px disabled:opacity-50"
            placeholder="Enter text here"
            onChange={event => {
              setIsSubmittable(event.currentTarget.value !== '')
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
            className="rounded-full w-40 px-2 bg-transparent border border-black/10 leading-170 h-6 py-px disabled:opacity-50"
            placeholder="Enter number here"
            onChange={event => {
              setIsSubmittable(event.currentTarget.value !== '')
              const value: number = event.currentTarget.valueAsNumber
              if (Number.isFinite(value)) {
                setState(value)
              }
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
            className="rounded-full w-40 px-2 bg-transparent border border-black/10 leading-170 h-6 py-px disabled:opacity-50"
            placeholder="Enter integer here"
            onChange={event => {
              setIsSubmittable(event.currentTarget.value !== '')
              const value: number = event.currentTarget.valueAsNumber
              if (Number.isFinite(value)) {
                setState(Math.floor(value))
              }
            }}
          />
        )
      }
      case 'object': {
        const propertiesObject =
          'properties' in schema ? object.asObject(schema.properties) ?? {} : {}
        const requiredProperties =
          'required' in schema && Array.isArray(schema.required) ? schema.required : []
        const propertyDefinitions = Object.entries(propertiesObject).flatMap(
          (kv: [string, unknown]) => {
            const [k, v] = kv
            return object
              .singletonObjectOrNull(v)
              .map(childSchema => ({ key: k, schema: childSchema }))
          }
        )
        const stateAsObject: Record<string, NonNullable<unknown> | null> =
          // This is SAFE, as `state` is an untyped object.
          // eslint-disable-next-line no-restricted-syntax
          (object.asObject(state) as Record<string, NonNullable<unknown> | null> | null) ??
          Object.fromEntries(
            propertyDefinitions.map(definition => [
              definition.key,
              constantValue(definition.schema)[0] ?? null,
            ])
          )
        if (initializing) {
          const value = constantValue(schema)
          if (!isSubmittable && value.length === 1) {
            setIsSubmittable(true)
          }
          if (state == null) {
            setState(stateAsObject)
          }
        }
        return constantValue(schema).length === 1 ? null : (
          <div className="flex flex-col gap-1 rounded-2xl border border-black/10 p-2">
            {propertyDefinitions.map((definition, i) => {
              const { key, schema: childSchema } = definition
              return constantValue(childSchema).length === 1 ? null : (
                <div key={key} className="flex flex-wrap items-center">
                  <div className="inline-block w-24">
                    {'title' in childSchema ? String(childSchema.title) : key}
                  </div>
                  <DataLinkWizard
                    schema={childSchema}
                    state={stateAsObject[key] ?? null}
                    setState={newValue => {
                      setState(oldState => ({ ...oldState, [key]: newValue }))
                    }}
                    setIsSubmittable={isChildSubmittable => {
                      setChildSubmittability(oldSubmittability =>
                        Array.from(propertyDefinitions, (childDefinition, j) =>
                          j === i
                            ? isChildSubmittable || !requiredProperties.includes(key)
                            : oldSubmittability?.[j] ??
                              constantValue(childDefinition.schema).length === 1
                        )
                      )
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
        setIsSubmittable(true)
        return <></>
      }
    }
  } else if ('$ref' in schema) {
    const referencedSchema = lookupDef(schema)
    if (referencedSchema == null) {
      setIsSubmittable(true)
      return <></>
    } else {
      return (
        <DataLinkWizard
          key={String(schema.$ref)}
          schema={referencedSchema}
          state={state}
          setState={setState}
          setIsSubmittable={setIsSubmittable}
        />
      )
    }
  } else if ('anyOf' in schema) {
    if (!Array.isArray(schema.anyOf)) {
      return <></>
    } else {
      const childSchemas = schema.anyOf.flatMap(object.singletonObjectOrNull)
      const selectedChildSchema = childSchemas[selectedChildIndex]
      const childValue = selectedChildSchema == null ? [] : constantValue(selectedChildSchema)
      if (initializing) {
        if (!isSubmittable && childValue.length === 1) {
          setIsSubmittable(true)
          setState(childValue[0])
        }
      }
      const dropdown = (
        <Dropdown
          items={childSchemas}
          selectedIndex={selectedChildIndex}
          render={childProps => getSchemaName(childProps.item)}
          className="self-start"
          onClick={(childSchema, index) => {
            setSelectedChildIndex(index)
            setState(null)
            setIsSubmittable(constantValue(childSchema).length === 1)
          }}
        />
      )
      return (
        <div className={`flex flex-col gap-1 ${childValue.length === 0 ? 'w-full' : ''}`}>
          {dropdownTitle != null ? (
            <div className="flex items-center">
              <div className="w-12 h-6 py-1">{dropdownTitle}</div>
              {dropdown}
            </div>
          ) : (
            dropdown
          )}
          {selectedChildSchema != null && childValue.length === 0 && (
            <DataLinkWizard
              schema={selectedChildSchema}
              state={state}
              setState={setState}
              setIsSubmittable={setIsSubmittable}
            />
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
            <DataLinkWizard
              key={i}
              schema={childSchema}
              state={state}
              setState={setState}
              setIsSubmittable={isChildSubmittable => {
                setChildSubmittability(oldSubmittability =>
                  Array.from(childSchemas, (otherChildSchema, j) =>
                    j === i
                      ? isChildSubmittable
                      : oldSubmittability?.[j] ?? constantValue(otherChildSchema).length === 1
                  )
                )
              }}
            />
          ))}
        </div>
      )
    }
  } else {
    return <></>
  }
}
