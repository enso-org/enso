/** @file A dynamic wizard for creating an arbitrary type of Datalink. */
import * as React from 'react'

import { Input, Text } from '#/components/aria'
import { Button, Dropdown } from '#/components/AriaComponents'
import Autocomplete from '#/components/Autocomplete'
import Checkbox from '#/components/styled/Checkbox'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'
import { useBackendQuery } from '#/hooks/backendHooks'
import { useRemoteBackendStrict } from '#/providers/BackendProvider'
import { useText } from '#/providers/TextProvider'
import { constantValue, getSchemaName, lookupDef } from '#/utilities/jsonSchema'
import { asObject, singletonObjectOrNull } from '#/utilities/object'
import { twMerge } from '#/utilities/tailwindMerge'

// =======================
// === JSONSchemaInput ===
// =======================

/** Props for a {@link JSONSchemaInput}. */
export interface JSONSchemaInputProps {
  readonly dropdownTitle?: string
  readonly defs: Record<string, object>
  readonly readOnly?: boolean
  readonly schema: object
  readonly path: string
  readonly getValidator: (path: string) => (value: unknown) => boolean
  readonly value: NonNullable<unknown> | null
  readonly setValue: React.Dispatch<React.SetStateAction<NonNullable<unknown> | null>>
}

/** A dynamic wizard for creating an arbitrary type of Datalink. */
export default function JSONSchemaInput(props: JSONSchemaInputProps) {
  const { dropdownTitle, readOnly = false, defs, schema, path, getValidator } = props
  const { value, setValue } = props
  // The functionality for inputting `enso-secret`s SHOULD be injected using a plugin,
  // but it is more convenient to avoid having plugin infrastructure.
  const remoteBackend = useRemoteBackendStrict()
  const { getText } = useText()
  const [autocompleteText, setAutocompleteText] = React.useState(() =>
    typeof value === 'string' ? value : null,
  )
  const [selectedChildIndex, setSelectedChildIndex] = React.useState<number | null>(null)
  const isSecret =
    'type' in schema &&
    schema.type === 'string' &&
    'format' in schema &&
    schema.format === 'enso-secret'
  const { data: secrets } = useBackendQuery(remoteBackend, 'listSecrets', [], { enabled: isSecret })
  const autocompleteItems = isSecret ? secrets?.map((secret) => secret.path) ?? null : null

  // NOTE: `enum` schemas omitted for now as they are not yet used.
  if ('const' in schema) {
    // This value cannot change.
    return null
  } else {
    const children: React.JSX.Element[] = []
    if ('type' in schema) {
      switch (schema.type) {
        case 'string': {
          if ('format' in schema && schema.format === 'enso-secret') {
            const isValid = typeof value === 'string' && value !== ''
            children.push(
              <div
                className={twMerge(
                  'w-60 rounded-default border-0.5',
                  isValid ? 'border-primary/20' : 'border-red-700/60',
                )}
              >
                <Autocomplete
                  items={autocompleteItems ?? []}
                  itemToKey={(item) => item}
                  itemToString={(item) => item}
                  placeholder={getText('enterSecretPath')}
                  matches={(item, text) => item.toLowerCase().includes(text.toLowerCase())}
                  values={isValid ? [value] : []}
                  setValues={(values) => {
                    setValue(values[0] ?? '')
                  }}
                  text={autocompleteText}
                  setText={setAutocompleteText}
                />
              </div>,
            )
          } else {
            children.push(
              <FocusArea direction="horizontal">
                {(innerProps) => (
                  <FocusRing>
                    <Input
                      type="text"
                      readOnly={readOnly}
                      value={typeof value === 'string' ? value : ''}
                      size={1}
                      className={twMerge(
                        'focus-child text w-60 grow rounded-input border-0.5 bg-transparent px-input-x read-only:read-only',
                        getValidator(path)(value) ? 'border-primary/20' : 'border-red-700/60',
                      )}
                      placeholder={getText('enterText')}
                      onChange={(event) => {
                        const newValue: string = event.currentTarget.value
                        setValue(newValue)
                      }}
                      {...innerProps}
                    />
                  </FocusRing>
                )}
              </FocusArea>,
            )
          }
          break
        }
        case 'number': {
          children.push(
            <FocusArea direction="horizontal">
              {(innerProps) => (
                <FocusRing>
                  <Input
                    type="number"
                    readOnly={readOnly}
                    value={typeof value === 'number' ? value : ''}
                    size={1}
                    className={twMerge(
                      'focus-child text w-60 grow rounded-input border-0.5 bg-transparent px-input-x read-only:read-only',
                      getValidator(path)(value) ? 'border-primary/20' : 'border-red-700/60',
                    )}
                    placeholder={getText('enterNumber')}
                    onChange={(event) => {
                      const newValue: number = event.currentTarget.valueAsNumber
                      if (Number.isFinite(newValue)) {
                        setValue(newValue)
                      }
                    }}
                    {...innerProps}
                  />
                </FocusRing>
              )}
            </FocusArea>,
          )
          break
        }
        case 'integer': {
          children.push(
            <FocusArea direction="horizontal">
              {(innerProps) => (
                <FocusRing>
                  <Input
                    type="number"
                    readOnly={readOnly}
                    value={typeof value === 'number' ? value : ''}
                    size={1}
                    className={twMerge(
                      'focus-child min-6- text40 w-80 grow rounded-input border-0.5 bg-transparent px-input-x read-only:read-only',
                      getValidator(path)(value) ? 'border-primary/20' : 'border-red-700/60',
                    )}
                    placeholder={getText('enterInteger')}
                    onChange={(event) => {
                      const newValue: number = Math.floor(event.currentTarget.valueAsNumber)
                      setValue(newValue)
                    }}
                    {...innerProps}
                  />
                </FocusRing>
              )}
            </FocusArea>,
          )
          break
        }
        case 'boolean': {
          children.push(
            <Checkbox
              isReadOnly={readOnly}
              isSelected={typeof value === 'boolean' && value}
              onChange={setValue}
            />,
          )
          break
        }
        case 'object': {
          const propertiesObject = 'properties' in schema ? asObject(schema.properties) ?? {} : {}
          const requiredProperties =
            'required' in schema && Array.isArray(schema.required) ? schema.required : []
          const propertyDefinitions = Object.entries(propertiesObject).flatMap(
            (kv: [string, unknown]) => {
              const [k, v] = kv
              return singletonObjectOrNull(v).map((childSchema) => ({
                key: k,
                schema: childSchema,
              }))
            },
          )
          if (constantValue(defs, schema).length !== 1) {
            children.push(
              <div className="grid items-center gap-json-schema rounded-default border-0.5 border-primary/20 p-json-schema-object-input">
                {propertyDefinitions.map((definition) => {
                  const { key, schema: childSchema } = definition
                  const isOptional = !requiredProperties.includes(key)
                  return constantValue(defs, childSchema).length === 1 ?
                      null
                    : <>
                        <FocusArea active={isOptional} direction="horizontal">
                          {(innerProps) => {
                            const isPresent = value != null && key in value
                            return (
                              <Button
                                size="custom"
                                variant="custom"
                                isDisabled={!isOptional}
                                isActive={!isOptional || isPresent}
                                className={twMerge(
                                  'col-start-1 inline-block whitespace-nowrap rounded-full px-button-x',
                                  isOptional && 'hover:bg-hover-bg',
                                )}
                                onPress={() => {
                                  if (isOptional) {
                                    setValue((oldValue) => {
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
                                        return {
                                          ...oldValue,
                                          [key]: constantValue(defs, childSchema, true)[0],
                                        }
                                      }
                                    })
                                  }
                                }}
                                {...innerProps}
                              >
                                {'title' in childSchema ? String(childSchema.title) : key}
                              </Button>
                            )
                          }}
                        </FocusArea>
                        {value != null && key in value && (
                          <div className="col-start-2">
                            <JSONSchemaInput
                              readOnly={readOnly}
                              defs={defs}
                              schema={childSchema}
                              path={`${path}/properties/${key}`}
                              getValidator={getValidator}
                              // This is SAFE, as `value` is an untyped object.
                              // eslint-disable-next-line no-restricted-syntax
                              value={(value as Record<string, unknown>)[key] ?? null}
                              setValue={(newValue) => {
                                setValue((oldValue) => {
                                  if (typeof newValue === 'function') {
                                    const unsafeValue: unknown = newValue(
                                      // This is SAFE; but there is no way to tell TypeScript that an object
                                      // has an index signature.
                                      // eslint-disable-next-line no-restricted-syntax
                                      (oldValue as Readonly<Record<string, unknown>>)[key] ?? null,
                                    )
                                    // The value MAY be `null`, but it is better than the value being a
                                    // function (which is *never* the intended result).
                                    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                                    newValue = unsafeValue!
                                  }
                                  return (
                                      typeof oldValue === 'object' &&
                                        oldValue != null &&
                                        // This is SAFE; but there is no way to tell TypeScript that an object
                                        // has an index signature.
                                        // eslint-disable-next-line no-restricted-syntax
                                        (oldValue as Readonly<Record<string, unknown>>)[key] ===
                                          newValue
                                    ) ?
                                      oldValue
                                    : { ...oldValue, [key]: newValue }
                                })
                              }}
                            />
                          </div>
                        )}
                      </>
                })}
              </div>,
            )
          }
          break
        }
      }
    }
    if ('$ref' in schema && typeof schema.$ref === 'string') {
      const referencedSchema = lookupDef(defs, schema)
      if (referencedSchema != null) {
        children.push(
          <JSONSchemaInput
            {...props}
            key={schema.$ref}
            schema={referencedSchema}
            path={schema.$ref}
          />,
        )
      }
    }
    if ('anyOf' in schema && Array.isArray(schema.anyOf)) {
      const childSchemas = schema.anyOf.flatMap(singletonObjectOrNull)
      const selectedChildSchema =
        selectedChildIndex == null ? null : childSchemas[selectedChildIndex]
      const selectedChildPath = `${path}/anyOf/${selectedChildIndex ?? 0}`
      const childValue = selectedChildSchema == null ? [] : constantValue(defs, selectedChildSchema)
      if (
        value != null &&
        (selectedChildSchema == null || getValidator(selectedChildPath)(value) !== true)
      ) {
        const newIndexRaw = childSchemas.findIndex((_, index) =>
          getValidator(`${path}/anyOf/${index}`)(value),
        )
        const newIndex = selectedChildSchema == null && newIndexRaw === -1 ? 0 : newIndexRaw
        if (newIndex !== -1 && newIndex !== selectedChildIndex) {
          setSelectedChildIndex(newIndex)
        }
      }
      const dropdown = (
        <FocusArea direction="horizontal">
          {(innerProps) => (
            <Dropdown
              readOnly={readOnly}
              items={childSchemas}
              selectedIndex={selectedChildIndex}
              render={(childProps) => <Text>{getSchemaName(defs, childProps.item)}</Text>}
              className="self-start"
              onChange={(childSchema, index) => {
                setSelectedChildIndex(index)
                const newConstantValue = constantValue(defs, childSchema, true)
                setValue(newConstantValue[0] ?? null)
              }}
              {...innerProps}
            />
          )}
        </FocusArea>
      )
      children.push(
        <div
          className={twMerge('flex flex-col gap-json-schema', childValue.length === 0 && 'w-full')}
        >
          {dropdownTitle != null ?
            <div className="flex h-row items-center">
              <div className="h-text w-json-schema-dropdown-title">{dropdownTitle}</div>
              {dropdown}
            </div>
          : dropdown}
          {selectedChildSchema != null && (
            <JSONSchemaInput
              key={selectedChildIndex}
              defs={defs}
              readOnly={readOnly}
              schema={selectedChildSchema}
              path={selectedChildPath}
              getValidator={getValidator}
              value={value}
              setValue={setValue}
            />
          )}
        </div>,
      )
    }
    if ('allOf' in schema && Array.isArray(schema.allOf)) {
      const childSchemas = schema.allOf.flatMap(singletonObjectOrNull)
      const newChildren = childSchemas.map((childSchema, i) => (
        <JSONSchemaInput
          key={i}
          defs={defs}
          readOnly={readOnly}
          schema={childSchema}
          path={`${path}/allOf/${i}`}
          getValidator={getValidator}
          value={value}
          setValue={setValue}
        />
      ))
      children.push(...newChildren)
    }
    return (
      children.length === 0 ? null
      : children.length === 1 && children[0] != null ? children[0]
      : <div className="flex flex-col gap-json-schema">{...children}</div>
    )
  }
}
