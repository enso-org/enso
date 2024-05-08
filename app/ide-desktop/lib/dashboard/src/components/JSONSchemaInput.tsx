/** @file A dynamic wizard for creating an arbitrary type of Data Link. */
import * as React from 'react'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import Autocomplete from '#/components/Autocomplete'
import Dropdown from '#/components/Dropdown'
import Checkbox from '#/components/styled/Checkbox'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'
import UnstyledButton from '#/components/UnstyledButton'

import * as jsonSchema from '#/utilities/jsonSchema'
import * as object from '#/utilities/object'

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

/** A dynamic wizard for creating an arbitrary type of Data Link. */
export default function JSONSchemaInput(props: JSONSchemaInputProps) {
  const { dropdownTitle, readOnly = false, defs, schema, path, getValidator } = props
  const { value: valueRaw, setValue: setValueRaw } = props
  // The functionality for inputting `enso-secret`s SHOULD be injected using a plugin,
  // but it is more convenient to avoid having plugin infrastructure.
  const remoteBackend = backendProvider.useRemoteBackend()
  const { getText } = textProvider.useText()
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
  } else {
    const children: JSX.Element[] = []
    if ('type' in schema) {
      switch (schema.type) {
        case 'string': {
          if ('format' in schema && schema.format === 'enso-secret') {
            const isValid = typeof value === 'string' && value !== ''
            if (autocompleteItems == null) {
              setAutocompleteItems([])
              void (async () => {
                const secrets = (await remoteBackend?.listSecrets()) ?? []
                setAutocompleteItems(secrets.map(secret => secret.path))
              })()
            }
            children.push(
              <div
                className={`grow rounded-default border ${
                  isValid ? 'border-primary/10' : 'border-red-700/60'
                }`}
              >
                <Autocomplete
                  items={autocompleteItems ?? []}
                  itemToKey={item => item}
                  itemToString={item => item}
                  placeholder={getText('enterSecretPath')}
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
            children.push(
              <FocusArea direction="horizontal">
                {innerProps => (
                  <FocusRing>
                    <aria.Input
                      type="text"
                      readOnly={readOnly}
                      value={typeof value === 'string' ? value : ''}
                      size={1}
                      className={`focus-child w-data-link-text-input text grow rounded-input border bg-transparent px-input-x read-only:read-only ${
                        getValidator(path)(value) ? 'border-primary/10' : 'border-red-700/60'
                      }`}
                      placeholder={getText('enterText')}
                      onChange={event => {
                        const newValue: string = event.currentTarget.value
                        setValue(newValue)
                      }}
                      {...innerProps}
                    />
                  </FocusRing>
                )}
              </FocusArea>
            )
          }
          break
        }
        case 'number': {
          children.push(
            <FocusArea direction="horizontal">
              {innerProps => (
                <FocusRing>
                  <aria.Input
                    type="number"
                    readOnly={readOnly}
                    value={typeof value === 'number' ? value : ''}
                    size={1}
                    className={`focus-child w-data-link-text-input text grow rounded-input border bg-transparent px-input-x read-only:read-only ${
                      getValidator(path)(value) ? 'border-primary/10' : 'border-red-700/60'
                    }`}
                    placeholder={getText('enterNumber')}
                    onChange={event => {
                      const newValue: number = event.currentTarget.valueAsNumber
                      if (Number.isFinite(newValue)) {
                        setValue(newValue)
                      }
                    }}
                    {...innerProps}
                  />
                </FocusRing>
              )}
            </FocusArea>
          )
          break
        }
        case 'integer': {
          children.push(
            <FocusArea direction="horizontal">
              {innerProps => (
                <FocusRing>
                  <aria.Input
                    type="number"
                    readOnly={readOnly}
                    value={typeof value === 'number' ? value : ''}
                    size={1}
                    className={`focus-child w-data-link-text-input text grow rounded-input border bg-transparent px-input-x read-only:read-only ${
                      getValidator(path)(value) ? 'border-primary/10' : 'border-red-700/60'
                    }`}
                    placeholder={getText('enterInteger')}
                    onChange={event => {
                      const newValue: number = Math.floor(event.currentTarget.valueAsNumber)
                      setValue(newValue)
                    }}
                    {...innerProps}
                  />
                </FocusRing>
              )}
            </FocusArea>
          )
          break
        }
        case 'boolean': {
          children.push(
            <Checkbox
              isReadOnly={readOnly}
              isSelected={typeof value === 'boolean' && value}
              onChange={setValue}
            />
          )
          break
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
          if (jsonSchema.constantValue(defs, schema).length !== 1) {
            children.push(
              <div className="flex flex-col gap-json-schema rounded-default border border-primary/10 p-json-schema-object-input">
                {propertyDefinitions.map(definition => {
                  const { key, schema: childSchema } = definition
                  const isOptional = !requiredProperties.includes(key)
                  return jsonSchema.constantValue(defs, childSchema).length === 1 ? null : (
                    <div
                      key={key}
                      className="flex flex-wrap items-center gap-buttons"
                      {...('description' in childSchema
                        ? { title: String(childSchema.description) }
                        : {})}
                    >
                      <FocusArea active={isOptional} direction="horizontal">
                        {innerProps => (
                          <UnstyledButton
                            isDisabled={!isOptional}
                            className={`text inline-block w-json-schema-object-key whitespace-nowrap rounded-full px-button-x text-left ${
                              isOptional ? 'hover:bg-hover-bg' : ''
                            }`}
                            onPress={() => {
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
                                    return {
                                      ...oldValue,
                                      [key]: jsonSchema.constantValue(defs, childSchema, true)[0],
                                    }
                                  }
                                })
                              }
                            }}
                            {...innerProps}
                          >
                            <aria.Text
                              className={`selectable ${
                                value != null && key in value ? 'active' : ''
                              }`}
                            >
                              {'title' in childSchema ? String(childSchema.title) : key}
                            </aria.Text>
                          </UnstyledButton>
                        )}
                      </FocusArea>
                      {value != null && key in value && (
                        <JSONSchemaInput
                          readOnly={readOnly}
                          defs={defs}
                          schema={childSchema}
                          path={`${path}/properties/${key}`}
                          getValidator={getValidator}
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
                              (oldValue as Readonly<Record<string, unknown>>)[key] === newValue
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
          break
        }
      }
    }
    if ('$ref' in schema && typeof schema.$ref === 'string') {
      const referencedSchema = jsonSchema.lookupDef(defs, schema)
      if (referencedSchema != null) {
        children.push(
          <JSONSchemaInput
            {...props}
            key={schema.$ref}
            schema={referencedSchema}
            path={schema.$ref}
          />
        )
      }
    }
    if ('anyOf' in schema && Array.isArray(schema.anyOf)) {
      const childSchemas = schema.anyOf.flatMap(object.singletonObjectOrNull)
      const selectedChildSchema =
        selectedChildIndex == null ? null : childSchemas[selectedChildIndex]
      const selectedChildPath = `${path}/anyOf/${selectedChildIndex}`
      const childValue =
        selectedChildSchema == null ? [] : jsonSchema.constantValue(defs, selectedChildSchema)
      if (
        value != null &&
        (selectedChildSchema == null || getValidator(selectedChildPath)(value) !== true)
      ) {
        const newIndexRaw = childSchemas.findIndex((_, index) =>
          getValidator(`${path}/anyOf/${index}`)(value)
        )
        const newIndex = selectedChildSchema == null && newIndexRaw === -1 ? 0 : newIndexRaw
        if (newIndex !== -1 && newIndex !== selectedChildIndex) {
          setSelectedChildIndex(newIndex)
        }
      }
      const dropdown = (
        <FocusArea direction="horizontal">
          {innerProps => (
            <Dropdown
              readOnly={readOnly}
              items={childSchemas}
              selectedIndex={selectedChildIndex}
              render={childProps => jsonSchema.getSchemaName(defs, childProps.item)}
              className="self-start"
              onClick={(childSchema, index) => {
                setSelectedChildIndex(index)
                const newConstantValue = jsonSchema.constantValue(defs, childSchema, true)
                setValue(newConstantValue[0] ?? null)
              }}
              {...innerProps}
            />
          )}
        </FocusArea>
      )
      children.push(
        <div className={`flex flex-col gap-json-schema ${childValue.length === 0 ? 'w-full' : ''}`}>
          {dropdownTitle != null ? (
            <div className="flex h-row items-center">
              <div className="h-text w-json-schema-dropdown-title">{dropdownTitle}</div>
              {dropdown}
            </div>
          ) : (
            dropdown
          )}
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
        </div>
      )
    }
    if ('allOf' in schema && Array.isArray(schema.allOf)) {
      const childSchemas = schema.allOf.flatMap(object.singletonObjectOrNull)
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
    return children.length === 0 ? null : children.length === 1 && children[0] != null ? (
      children[0]
    ) : (
      <div className="flex flex-col gap-json-schema">{...children}</div>
    )
  }
}
