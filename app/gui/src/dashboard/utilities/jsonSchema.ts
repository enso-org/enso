/** @file Utilities for using JSON schemas. */
import * as objectModule from '#/utilities/object'

// =================
// === lookupDef ===
// =================

/** Look up a `{ "$ref": "" }` in the root schema. */
export function lookupDef(defs: Record<string, object>, schema: object) {
  const ref = '$ref' in schema && typeof schema.$ref === 'string' ? schema.$ref : null
  const [, name] = ref?.match(/^#[/][$]defs[/](.+)$/) ?? ''
  return name == null ? null : objectModule.asObject(defs[name])
}

// =====================
// === getSchemaName ===
// =====================

const SCHEMA_NAMES = new WeakMap<object, string>()

/** Return a human-readable name representing a schema. */
function getSchemaNameHelper(defs: Record<string, object>, schema: object): string {
  if ('title' in schema) {
    return String(schema.title)
  } else if ('type' in schema) {
    return String(schema.type)
  } else if ('$ref' in schema) {
    const referencedSchema = lookupDef(defs, schema)
    return referencedSchema == null ? '(unknown)' : getSchemaName(defs, referencedSchema)
  } else if ('anyOf' in schema) {
    const members = Array.isArray(schema.anyOf) ? schema.anyOf : []
    return (
      members
        .flatMap(objectModule.singletonObjectOrNull)
        .map((childSchema) => getSchemaName(defs, childSchema))
        .join(' | ') || '(unknown)'
    )
  } else if ('allOf' in schema) {
    const members = Array.isArray(schema.allOf) ? schema.allOf : []
    return (
      members
        .flatMap(objectModule.singletonObjectOrNull)
        .map((childSchema) => getSchemaName(defs, childSchema))
        .join(' & ') || '(unknown)'
    )
  } else {
    return '(unknown)'
  }
}

/**
 * Return a human-readable name representing a schema.
 * This function is a memoized version of {@link getSchemaNameHelper}.
 */
export function getSchemaName(defs: Record<string, object>, schema: object) {
  const cached = SCHEMA_NAMES.get(schema)
  if (cached != null) {
    return cached
  } else {
    const name = getSchemaNameHelper(defs, schema)
    SCHEMA_NAMES.set(schema, name)
    return name
  }
}

// =============================
// === constantValueToSchema ===
// =============================

/**
 * Convert a constant value to a JSON schema representing the value, or `null` if it cannot be
 * represented.
 */
export function constantValueToSchema(value: unknown): object | null {
  let result: object | null
  switch (typeof value) {
    case 'string':
    case 'number':
    case 'boolean': {
      if (typeof value === 'number' && !Number.isFinite(value)) {
        result = null
      } else {
        // Note that `NaN`, `Infinity` and `-Infinity` are not represntable in JSON schema.
        result = { const: value, type: typeof value }
      }
      break
    }
    case 'object': {
      if (value == null) {
        result = { type: 'null' }
      } else if (Array.isArray(value)) {
        const prefixItems: object[] = []
        result = {
          type: 'array',
          ...(value.length === 0 ? {} : { prefixItems }),
          minItems: value.length,
          maxItems: value.length,
          items: false,
        }
        for (const child of value) {
          const schema = constantValueToSchema(child)
          if (schema == null) {
            result = null
            break
          }
          prefixItems.push(schema)
        }
      } else {
        const properties: Record<string, object> = {}
        result = { type: 'object', properties, required: Object.keys(value) }
        for (const [key, childValue] of Object.entries(value)) {
          const schema = constantValueToSchema(childValue)
          if (schema == null) {
            result = null
            break
          }
          Object.defineProperty(properties, key, { value: schema, enumerable: true })
        }
      }
      break
    }
    case 'bigint': {
      // Non-standard.
      result = { const: String(value), type: 'string', format: 'bigint' }
      break
    }
    case 'symbol':
    case 'function':
    case 'undefined': {
      // Not possible to represent in JSON schema - they will be replaced with the schema for
      // `null`.
      result = null
      break
    }
  }
  return result
}

// =====================
// === constantValue ===
// =====================

const CONSTANT_VALUE = new WeakMap<object, readonly [] | readonly [NonNullable<unknown> | null]>()
const PARTIAL_CONSTANT_VALUE = new WeakMap<
  object,
  readonly [] | readonly [NonNullable<unknown> | null]
>()
const SINGLETON_NULL = Object.freeze([null] as const)
const EMPTY_ARRAY = Object.freeze([] as const)

// FIXME: Adjust to allow `type` and `anyOf` and `allOf` and `$ref` to all be present
/** The value of the schema, if it can only have one possible value. */
function constantValueOfSchemaHelper(
  defs: Record<string, object>,
  schema: object,
  partial = false,
): readonly [] | readonly [NonNullable<unknown> | null] {
  if ('const' in schema) {
    return [schema.const ?? null]
  } else {
    const invalid: readonly [] | readonly [NonNullable<unknown> | null] =
      partial ? SINGLETON_NULL : EMPTY_ARRAY
    const results: (NonNullable<unknown> | null)[] = []
    if ('default' in schema && schema.default != null && partial) {
      return [schema.default]
    } else if ('type' in schema) {
      switch (schema.type) {
        case 'null': {
          results.push(null)
          break
        }
        case 'object': {
          const propertiesObject =
            'properties' in schema ? objectModule.asObject(schema.properties) ?? {} : {}
          const required = new Set(
            'required' in schema && Array.isArray(schema.required) ?
              schema.required.map(String)
            : [],
          )
          const object: Record<string, unknown> = {}
          results.push(object)
          for (const [key, child] of Object.entries(propertiesObject)) {
            const childSchema = objectModule.asObject(child)
            if (childSchema == null || (partial && !required.has(key))) {
              continue
            }
            const value = constantValueOfSchema(defs, childSchema, partial)
            if (value.length === 0 && !partial) {
              return invalid
            } else {
              Object.defineProperty(object, key, { value: value[0] ?? null, enumerable: true })
            }
          }
          break
        }
        case 'array': {
          if (!partial && (!('items' in schema) || schema.items !== false)) {
            // This array may contain extra items.
            return invalid
          } else if (!('prefixItems' in schema) || !Array.isArray(schema.prefixItems)) {
            results.push([])
            break
          } else {
            const array: unknown[] = []
            results.push(array)
            for (const childSchema of schema.prefixItems) {
              const childSchemaObject = objectModule.asObject(childSchema)
              const childValue =
                childSchemaObject == null ?
                  []
                : constantValueOfSchema(defs, childSchemaObject, partial)
              if (childValue.length === 0 && !partial) {
                return invalid
              }
              array.push(childValue[0] ?? null)
            }
            break
          }
        }
      }
    } else if ('$ref' in schema) {
      const referencedSchema = lookupDef(defs, schema)
      if (referencedSchema == null) {
        return invalid
      } else {
        const value = constantValueOfSchema(defs, referencedSchema, partial)
        if (!partial && value.length === 0) {
          return invalid
        }
        if (value.length === 1) {
          results.push(value[0])
        }
      }
    } else if ('anyOf' in schema) {
      if (!Array.isArray(schema.anyOf) || (!partial && schema.anyOf.length !== 1)) {
        return invalid
      } else {
        const firstMember = objectModule.asObject(schema.anyOf[0])
        if (firstMember == null) {
          return invalid
        } else {
          const value = constantValueOfSchema(defs, firstMember, partial)
          if (!partial && value.length === 0) {
            return invalid
          }
          if (value.length === 1) {
            results.push(value[0])
          }
        }
      }
    }
    if ('allOf' in schema && Array.isArray(schema.allOf)) {
      if (schema.allOf.length === 0) {
        return invalid
      } else {
        for (const childSchema of schema.allOf) {
          const schemaObject = objectModule.asObject(childSchema)
          const value =
            schemaObject == null ? [] : constantValueOfSchema(defs, schemaObject, partial)
          if (!partial && value.length === 0) {
            return invalid
          }
          if (value.length === 1) {
            results.push(value[0])
          }
        }
      }
    }
    if (partial && results.length === 0) {
      if ('type' in schema) {
        switch (schema.type) {
          case 'string': {
            return ['']
          }
          case 'number':
          case 'integer': {
            return [0]
          }
          case 'boolean': {
            return [true]
          }
          default: {
            return SINGLETON_NULL
          }
        }
      } else {
        return SINGLETON_NULL
      }
    } else if (results.length === 0) {
      return invalid
    } else {
      const result = results[0] ?? null
      let resultArray: readonly [] | readonly [NonNullable<unknown> | null] = [result]
      for (const child of results.slice(1)) {
        const childSchema = objectModule.asObject(child)
        if (childSchema == null) {
          continue
        }
        const value = constantValueOfSchema(defs, childSchema, partial)
        if (value.length === 0 && !partial) {
          resultArray = []
          break
        } else if (typeof result !== 'object' || result == null) {
          if (result !== value[0] && !partial) {
            resultArray = []
            break
          }
        } else {
          if (value[0] == null || (typeof result !== typeof value[0] && !partial)) {
            resultArray = []
            break
          }
          Object.assign(result, value[0])
        }
      }
      if (partial && 'type' in schema) {
        switch (schema.type) {
          case 'string':
          case 'number':
          case 'boolean': {
            return typeof resultArray[0] === schema.type ? resultArray : invalid
          }
          case 'integer': {
            return typeof resultArray[0] === 'number' && Number.isInteger(resultArray[0]) ?
                resultArray
              : invalid
          }
          default: {
            return resultArray
          }
        }
      } else {
        return resultArray
      }
    }
  }
}

/**
 * The value of the schema, if it can only have one possible value.
 * This function is a memoized version of {@link constantValueOfSchemaHelper}.
 */
export function constantValueOfSchema(
  defs: Record<string, object>,
  schema: object,
  partial = false,
) {
  const cache = partial ? PARTIAL_CONSTANT_VALUE : CONSTANT_VALUE
  const cached = cache.get(schema)
  if (cached != null) {
    return cached
  } else {
    const renderable = constantValueOfSchemaHelper(defs, schema, partial)
    cache.set(schema, renderable)
    return renderable
  }
}
