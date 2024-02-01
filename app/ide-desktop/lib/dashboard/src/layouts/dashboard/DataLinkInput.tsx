/** @file A dynamic wizard for creating an arbitrary type of Data Link. */
import * as React from 'react'

import SCHEMA from '#/data/dataLinkSchema.json' assert { type: 'json' }

import Dropdown from '#/components/Dropdown'

import * as jsonSchema from '#/utilities/jsonSchema'
import * as object from '#/utilities/object'

// =================
// === Constants ===
// =================

const DEFS: Record<string, object> = SCHEMA.$defs

// =====================
// === constantValue ===
// =====================

/** The value of the schema, if it can only have one possible value. */
function constantValue(schema: object, partial = false) {
  return jsonSchema.constantValue(DEFS, schema, partial)
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
export interface DataLinkInputProps {
  dropdownTitle?: string
  schema?: object
  readOnly?: boolean
  value: NonNullable<unknown> | null
  setValue: React.Dispatch<React.SetStateAction<NonNullable<unknown> | null>>
  setIsSubmittable: (isSubmittable: boolean) => void
}

/** A dynamic wizard for creating an arbitrary type of Data Link. */
export default function DataLinkInput(props: DataLinkInputProps) {
  const { dropdownTitle, schema = SCHEMA.$defs.DataLink, readOnly = false, value: valueRaw } = props
  const { setValue: setValueRaw, setIsSubmittable: setIsSubmittableRaw } = props
  const [selectedChildIndex, setSelectedChildIndex] = React.useState<number | null>(null)
  const [isSubmittable, setIsSubmittable] = React.useState(false)
  const [childSubmittability, setChildSubmittability] = React.useState<boolean[] | null>(null)
  const [value, setValue] = React.useState(valueRaw)

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
    setValue(valueRaw)
    // `initializing` is not a dependency.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [valueRaw])

  React.useEffect(() => {
    setValueRaw(value)
    // `setStateRaw` is a callback, not a dependency.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [value])

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
            readOnly={readOnly}
            value={typeof value === 'string' ? value : ''}
            size={1}
            className="rounded-full w-40 px-2 bg-transparent border border-black/10 leading-170 h-6 py-px disabled:opacity-50 read-only:opacity-75 read-only:cursor-not-allowed"
            placeholder="Enter text here"
            onChange={event => {
              setIsSubmittable(
                event.currentTarget.value !== '' &&
                  jsonSchema.isMatch(DEFS, schema, event.currentTarget.value)
              )
              const newValue: string = event.currentTarget.value
              setValue(newValue)
            }}
          />
        )
      }
      case 'number': {
        return (
          <input
            type="number"
            readOnly={readOnly}
            value={typeof value === 'number' ? value : ''}
            size={1}
            className="rounded-full w-40 px-2 bg-transparent border border-black/10 leading-170 h-6 py-px disabled:opacity-50 read-only:opacity-75 read-only:cursor-not-allowed"
            placeholder="Enter number here"
            onChange={event => {
              const newValue: number = event.currentTarget.valueAsNumber
              if (Number.isFinite(newValue)) {
                setIsSubmittable(
                  event.currentTarget.value !== '' && jsonSchema.isMatch(DEFS, schema, newValue)
                )
                setValue(newValue)
              }
            }}
          />
        )
      }
      case 'integer': {
        return (
          <input
            type="number"
            readOnly={readOnly}
            value={typeof value === 'number' ? value : ''}
            size={1}
            className="rounded-full w-40 px-2 bg-transparent border border-black/10 leading-170 h-6 py-px disabled:opacity-50 read-only:opacity-75 read-only:cursor-not-allowed"
            placeholder="Enter integer here"
            onChange={event => {
              const newValue: number = Math.floor(event.currentTarget.valueAsNumber)
              if (Number.isFinite(newValue)) {
                setIsSubmittable(
                  event.currentTarget.value !== '' && jsonSchema.isMatch(DEFS, schema, newValue)
                )
                setValue(newValue)
              }
            }}
          />
        )
      }
      case 'boolean': {
        return (
          <input
            type="checkbox"
            readOnly={readOnly}
            checked={typeof value === 'boolean' && value}
            onChange={event => {
              setValue(event.currentTarget.checked)
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
          (object.asObject(value) ?? {}) as Record<string, NonNullable<unknown> | null>
        if (childSubmittability == null) {
          setChildSubmittability(
            Array.from(propertyDefinitions, childDefinition =>
              jsonSchema.isMatch(DEFS, childDefinition.schema, stateAsObject[childDefinition.key])
            )
          )
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
                  <DataLinkInput
                    readOnly={readOnly}
                    schema={childSchema}
                    value={stateAsObject[key] ?? null}
                    setValue={newValue => {
                      setValue(oldState =>
                        typeof oldState === 'object' &&
                        oldState != null &&
                        // This is SAFE; but there is no way to tell TypeScript that an object
                        // has an index signature.
                        // eslint-disable-next-line no-restricted-syntax
                        (oldState as Record<string, unknown>)[key] === newValue
                          ? oldState
                          : { ...oldState, [key]: newValue }
                      )
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
    const referencedSchema = jsonSchema.lookupDef(DEFS, schema)
    if (referencedSchema == null) {
      setIsSubmittable(true)
      return <></>
    } else {
      return (
        <DataLinkInput
          key={String(schema.$ref)}
          readOnly={readOnly}
          schema={referencedSchema}
          value={value}
          setValue={setValue}
          setIsSubmittable={setIsSubmittable}
        />
      )
    }
  } else if ('anyOf' in schema) {
    if (!Array.isArray(schema.anyOf)) {
      return <></>
    } else {
      const childSchemas = schema.anyOf.flatMap(object.singletonObjectOrNull)
      const selectedChildSchema =
        selectedChildIndex == null ? null : childSchemas[selectedChildIndex]
      const childValue = selectedChildSchema == null ? [] : constantValue(selectedChildSchema)
      if (
        value != null &&
        (selectedChildSchema == null ||
          !jsonSchema.isMatch(DEFS, selectedChildSchema, value, { partial: true }))
      ) {
        const newIndex = childSchemas.findIndex(childSchema =>
          jsonSchema.isMatch(DEFS, childSchema, value, { partial: true })
        )
        if (newIndex !== -1 && newIndex !== selectedChildIndex) {
          setSelectedChildIndex(newIndex)
        }
      }
      const dropdown = (
        <Dropdown
          readOnly={readOnly}
          items={childSchemas}
          selectedIndex={selectedChildIndex}
          render={childProps => getSchemaName(childProps.item)}
          className="self-start"
          onClick={(childSchema, index) => {
            setSelectedChildIndex(index)
            const newConstantValue = constantValue(childSchema, true)
            setValue(newConstantValue[0] ?? null)
            setIsSubmittable(newConstantValue.length === 1)
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
          {selectedChildSchema != null && (
            <DataLinkInput
              key={selectedChildIndex}
              readOnly={readOnly}
              schema={selectedChildSchema}
              value={value}
              setValue={setValue}
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
            <DataLinkInput
              key={i}
              readOnly={readOnly}
              schema={childSchema}
              value={value}
              setValue={setValue}
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
