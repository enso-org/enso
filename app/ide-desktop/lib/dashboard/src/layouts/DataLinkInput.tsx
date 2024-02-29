/** @file A dynamic wizard for creating an arbitrary type of Data Link. */
import * as React from 'react'

import SCHEMA from '#/data/dataLinkSchema.json' assert { type: 'json' }

import * as backendProvider from '#/providers/BackendProvider'

import Autocomplete from '#/components/Autocomplete'
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
  readonly dropdownTitle?: string
  readonly schema?: object
  readonly readOnly?: boolean
  readonly value: NonNullable<unknown> | null
  readonly setValue: React.Dispatch<React.SetStateAction<NonNullable<unknown> | null>>
}

/** A dynamic wizard for creating an arbitrary type of Data Link. */
export default function DataLinkInput(props: DataLinkInputProps) {
  const { dropdownTitle, schema = SCHEMA.$defs.DataLink, readOnly = false, value: valueRaw } = props
  const { setValue: setValueRaw } = props
  const { backend } = backendProvider.useBackend()
  const [value, setValue] = React.useState(valueRaw)
  const [autocompleteText, setAutocompleteText] = React.useState(() =>
    typeof value === 'string' ? value : null
  )
  const [selectedChildIndex, setSelectedChildIndex] = React.useState<number | null>(null)
  const [autocompleteItems, setAutocompleteItems] = React.useState<string[] | null>(null)

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
        if ('format' in schema && schema.format === 'enso-secret') {
          const isValid = typeof value === 'string' && value !== ''
          if (autocompleteItems == null) {
            setAutocompleteItems([])
            void (async () => {
              const secrets = await backend.listSecrets()
              // FIXME: Extract secret path instead of ID.
              setAutocompleteItems(secrets.map(secret => secret.id))
            })()
          }
          return (
            <div
              className={`rounded-default border ${
                isValid ? 'border-black/10' : 'border-red-700/60'
              }`}
            >
              <Autocomplete
                items={autocompleteItems ?? []}
                itemToKey={item => item}
                itemToString={item => item}
                placeholder="Enter secret path"
                matches={(item, text) => item.toLowerCase().includes(text.toLowerCase())}
                values={isValid ? [value] : []}
                setValues={values => {
                  setValue(values[0])
                }}
                text={autocompleteText}
                setText={setAutocompleteText}
              />
            </div>
          )
        } else {
          return (
            <input
              type="text"
              readOnly={readOnly}
              value={typeof value === 'string' ? value : ''}
              size={1}
              className={`rounded-full w-data-link-text-input px-input-x bg-transparent border text disabled:opacity-disabled read-only:opacity-read-only read-only:cursor-not-allowed ${
                jsonSchema.isMatch(DEFS, schema, value) ? 'border-black/10' : 'border-red-700/60'
              }`}
              placeholder="Enter text"
              onChange={event => {
                const newValue: string = event.currentTarget.value
                setValue(newValue)
              }}
            />
          )
        }
      }
      case 'number': {
        return (
          <input
            type="number"
            readOnly={readOnly}
            value={typeof value === 'number' ? value : ''}
            size={1}
            className={`rounded-full w-data-link-text-input px-input-x bg-transparent border text disabled:opacity-disabled read-only:opacity-read-only read-only:cursor-not-allowed ${
              jsonSchema.isMatch(DEFS, schema, value) ? 'border-black/10' : 'border-red-700/60'
            }`}
            placeholder="Enter number"
            onChange={event => {
              const newValue: number = event.currentTarget.valueAsNumber
              if (Number.isFinite(newValue)) {
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
            className={`rounded-full w-data-link-text-input px-input-x bg-transparent border text disabled:opacity-disabled read-only:opacity-read-only read-only:cursor-not-allowed ${
              jsonSchema.isMatch(DEFS, schema, value) ? 'border-black/10' : 'border-red-700/60'
            }`}
            placeholder="Enter integer"
            onChange={event => {
              const newValue: number = Math.floor(event.currentTarget.valueAsNumber)
              if (Number.isFinite(newValue)) {
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
        return constantValue(schema).length === 1 ? null : (
          <div className="flex flex-col gap-data-link rounded-default border border-black/10 p-data-link-object-input">
            {propertyDefinitions.map(definition => {
              const { key, schema: childSchema } = definition
              const isOptional = !requiredProperties.includes(key)
              return constantValue(childSchema).length === 1 ? null : (
                <div
                  key={key}
                  className="flex flex-wrap items-center"
                  {...('description' in childSchema
                    ? { title: String(childSchema.description) }
                    : {})}
                >
                  <div
                    className={`text inline-block w-data-link-object-input whitespace-nowrap ${
                      isOptional ? 'cursor-pointer' : ''
                    } ${value != null && key in value ? '' : 'opacity-disabled'}`}
                    onClick={() => {
                      if (isOptional) {
                        setValue(oldValue => {
                          if (oldValue != null && key in oldValue) {
                            // This is SAFE, as `value` is an untyped object.
                            // The removed key is intentionally unused.
                            // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unused-vars
                            const { [key]: removed, ...newValue } = oldValue as Record<
                              string,
                              NonNullable<unknown> | null
                            >
                            return newValue
                          } else {
                            return { ...oldValue, [key]: constantValue(childSchema, true)[0] }
                          }
                        })
                      }
                    }}
                  >
                    {'title' in childSchema ? String(childSchema.title) : key}
                  </div>
                  {value != null && key in value && (
                    <DataLinkInput
                      readOnly={readOnly}
                      schema={childSchema}
                      // This is SAFE, as `value` is an untyped object.
                      // eslint-disable-next-line no-restricted-syntax
                      value={(value as Record<string, unknown>)[key] ?? null}
                      setValue={newValue => {
                        setValue(oldValue =>
                          typeof oldValue === 'object' &&
                          oldValue != null &&
                          // This is SAFE; but there is no way to tell TypeScript that an object
                          // has an index signature.
                          // eslint-disable-next-line no-restricted-syntax
                          (oldValue as Record<string, unknown>)[key] === newValue
                            ? oldValue
                            : { ...oldValue, [key]: newValue }
                        )
                      }}
                    />
                  )}
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
    const referencedSchema = jsonSchema.lookupDef(DEFS, schema)
    if (referencedSchema == null) {
      return <></>
    } else {
      return (
        <DataLinkInput
          key={String(schema.$ref)}
          readOnly={readOnly}
          schema={referencedSchema}
          value={value}
          setValue={setValue}
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
          }}
        />
      )
      return (
        <div className={`flex flex-col gap-data-link ${childValue.length === 0 ? 'w-full' : ''}`}>
          {dropdownTitle != null ? (
            <div className="flex items-center h-row">
              <div className="w-data-link-dropdown-title h-text">{dropdownTitle}</div>
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
        <div className="flex flex-col gap-data-link">
          {childSchemas.map((childSchema, i) => (
            <DataLinkInput
              key={i}
              readOnly={readOnly}
              schema={childSchema}
              value={value}
              setValue={setValue}
            />
          ))}
        </div>
      )
    }
  } else {
    return <></>
  }
}
