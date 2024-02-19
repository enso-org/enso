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

// =============================
// === constantValueToSchema ===
// =============================

/** Convert a constant value to a JSON schema representing the value, or `null` if it cannot be
 * represented. */
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
        result = { type: 'array', prefixItems, items: false }
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

const CONSTANT_VALUE = new WeakMap<object, [] | [NonNullable<unknown> | null]>()
const PARTIAL_CONSTANT_VALUE = new WeakMap<object, [] | [NonNullable<unknown> | null]>()

/** The value of the schema, if it can only have one possible value. */
function constantValueHelper(
  defs: Record<string, object>,
  schema: object,
  partial = false
): [] | [NonNullable<unknown> | null] {
  let result: [] | [NonNullable<unknown> | null]
  if ('const' in schema) {
    result = [schema.const ?? null]
  } else if ('type' in schema) {
    switch (schema.type) {
      case 'string':
      case 'number':
      case 'integer':
      case 'boolean': {
        // These should already be covered by the `const` check above.
        result = []
        if (partial) {
          switch (schema.type) {
            case 'string': {
              result = ['']
              break
            }
            case 'number':
            case 'integer': {
              result = [0]
              break
            }
            case 'boolean': {
              result = [false]
              break
            }
          }
        }
        break
      }
      case 'null': {
        result = [null]
        break
      }
      case 'object': {
        const propertiesObject =
          'properties' in schema ? objectModule.asObject(schema.properties) ?? {} : {}
        const required = new Set(
          'required' in schema && Array.isArray(schema.required) ? schema.required.map(String) : []
        )
        const object: Record<string, unknown> = {}
        result = [object]
        for (const [key, child] of Object.entries(propertiesObject)) {
          const childSchema = objectModule.asObject(child)
          if (childSchema == null || (partial && !required.has(key))) {
            continue
          }
          const value = constantValue(defs, childSchema, partial)
          if (value.length === 0 && !partial) {
            // eslint-disable-next-line no-restricted-syntax
            result = []
            break
          } else {
            Object.defineProperty(object, key, { value: value[0] ?? null, enumerable: true })
          }
        }
        break
      }
      case 'array': {
        if (!('items' in schema) || schema.items !== false) {
          // This array may contain extra items.
          result = []
          break
        } else if (!('prefixItems' in schema) || !Array.isArray(schema.prefixItems)) {
          // Invalid format.
          result = []
          break
        } else {
          const array: unknown[] = []
          result = [array]
          for (const childSchema of schema.prefixItems) {
            const childSchemaObject = objectModule.asObject(childSchema)
            const childValue =
              childSchemaObject == null ? [] : constantValue(defs, childSchemaObject, partial)
            if (childValue.length === 0 && !partial) {
              result = []
              break
            }
            array.push(childValue[0] ?? null)
          }
          break
        }
      }
      default: {
        result = []
        break
      }
    }
  } else if ('$ref' in schema) {
    const referencedSchema = lookupDef(defs, schema)
    result = referencedSchema == null ? [] : constantValue(defs, referencedSchema, partial)
  } else if ('anyOf' in schema) {
    if (!Array.isArray(schema.anyOf) || (!partial && schema.anyOf.length !== 1)) {
      result = []
    } else {
      const firstMember = objectModule.asObject(schema.anyOf[0])
      result = firstMember == null ? [] : constantValue(defs, firstMember, partial)
    }
  } else if ('allOf' in schema) {
    if (!Array.isArray(schema.allOf) || schema.allOf.length === 0) {
      result = []
    } else {
      const firstMember = objectModule.asObject(schema.allOf[0])
      const firstValue = firstMember == null ? [] : constantValue(defs, firstMember, partial)
      if (firstValue.length === 0) {
        result = []
      } else {
        const intersection = firstValue[0]
        result = [intersection]
        for (const child of schema.allOf.slice(1)) {
          const childSchema = objectModule.asObject(child)
          if (childSchema == null) {
            continue
          }
          const value = constantValue(defs, childSchema, partial)
          if (value.length === 0 && !partial) {
            result = []
            break
          } else if (typeof intersection !== 'object' || intersection == null) {
            if (intersection !== value[0] && !partial) {
              result = []
              break
            }
          } else {
            if (value[0] == null || (typeof intersection !== typeof value[0] && !partial)) {
              result = []
              break
            }
            Object.assign(intersection, value[0])
          }
        }
      }
    }
  } else {
    result = []
  }
  return partial && result.length === 0 ? [null] : result
}

/** The value of the schema, if it can only have one possible value.
 * This function is a memoized version of {@link constantValueHelper}. */
export function constantValue(defs: Record<string, object>, schema: object, partial = false) {
  const cache = partial ? PARTIAL_CONSTANT_VALUE : CONSTANT_VALUE
  const cached = cache.get(schema)
  if (cached != null) {
    return cached
  } else {
    const renderable = constantValueHelper(defs, schema, partial)
    cache.set(schema, renderable)
    return renderable
  }
}

// ===============
// === isMatch ===
// ===============

/** Options for {@link isMatch}. */
export interface MatchOptions {
  /** If true, accept a match where one or more members are `null`, `undefined`, or not present. */
  readonly partial?: boolean
}

/** Attempt to construct a RegExp from the given pattern. If that fails, return a regex that matches
 * any string. */
function tryRegExp(pattern: string) {
  try {
    return new RegExp(pattern)
  } catch {
    return new RegExp('')
  }
}

/** Whether the value complies with the schema.. */
export function isMatch(
  defs: Record<string, object>,
  schema: object,
  value: unknown,
  options: MatchOptions = {}
): boolean {
  const { partial = false } = options
  let result: boolean
  if (partial && value == null) {
    result = true
  } else if ('const' in schema) {
    result = schema.const === value
  } else if ('type' in schema) {
    switch (schema.type) {
      case 'string': {
        // https://json-schema.org/understanding-json-schema/reference/string
        if (typeof value !== 'string') {
          result = false
        } else if (partial && value === '') {
          result = true
        } else if (
          'minLength' in schema &&
          typeof schema.minLength === 'number' &&
          value.length < schema.minLength
        ) {
          result = false
        } else if (
          'maxLength' in schema &&
          typeof schema.maxLength === 'number' &&
          value.length > schema.maxLength
        ) {
          result = false
        } else if (
          'pattern' in schema &&
          typeof schema.pattern === 'string' &&
          !tryRegExp(schema.pattern).test(value)
        ) {
          result = false
        } else {
          const format =
            'format' in schema && typeof schema.format === 'string' ? schema.format : null
          // `format` validation has been omitted as it is currently not needed, and quite complex
          // to correctly validate.
          // https://json-schema.org/understanding-json-schema/reference/string#built-in-formats
          result = true
          switch (format) {
            case null:
            default: {
              break
            }
          }
        }
        break
      }
      case 'number':
      case 'integer': {
        // https://json-schema.org/understanding-json-schema/reference/numeric
        if (typeof value !== 'number') {
          result = false
        } else if (partial && value === 0) {
          result = true
        } else if (schema.type === 'integer' && !Number.isInteger(value)) {
          result = false
        } else if (
          'multipleOf' in schema &&
          typeof schema.multipleOf === 'number' &&
          value !== 0 &&
          value % schema.multipleOf !== 0 &&
          // Should be mostly equivalent to `%`, except more robust for multiple detection
          // in some cases like`1 % 0.01`.
          value - schema.multipleOf * Math.round(value / schema.multipleOf) !== 0
        ) {
          result = false
        } else if (
          'minimum' in schema &&
          typeof schema.minimum === 'number' &&
          value < schema.minimum
        ) {
          result = false
        } else if (
          'exclusiveMinimum' in schema &&
          typeof schema.exclusiveMinimum === 'number' &&
          value <= schema.exclusiveMinimum
        ) {
          result = false
        } else if (
          'maximum' in schema &&
          typeof schema.maximum === 'number' &&
          value > schema.maximum
        ) {
          result = false
        } else if (
          'exclusiveMaximum' in schema &&
          typeof schema.exclusiveMaximum === 'number' &&
          value >= schema.exclusiveMaximum
        ) {
          result = false
        } else {
          result = true
        }
        break
      }
      case 'boolean': {
        result = typeof value === 'boolean'
        break
      }
      case 'null': {
        // This MUST only match `null` and not `undefined`.
        // eslint-disable-next-line eqeqeq
        result = value === null
        break
      }
      case 'object': {
        if (typeof value !== 'object' || value == null) {
          result = false
        } else {
          // This is SAFE, since arbitrary properties are technically valid on objects.
          // eslint-disable-next-line no-restricted-syntax
          const valueObject = value as Record<string, unknown>
          const propertiesObject =
            'properties' in schema ? objectModule.asObject(schema.properties) ?? {} : {}
          const required = new Set(
            'required' in schema && Array.isArray(schema.required)
              ? schema.required.map(String)
              : []
          )
          result = Object.entries(propertiesObject).every(kv => {
            // This is SAFE, as it is safely converted to an `object` on the next line.
            // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
            const [key, childSchema] = kv
            const childSchemaObject = objectModule.asObject(childSchema)
            return (
              (key in valueObject &&
                childSchemaObject != null &&
                isMatch(defs, childSchemaObject, valueObject[key], options)) ||
              (!(key in valueObject) && !required.has(key))
            )
          })
        }
        break
      }
      case 'array': {
        let startIndex = 0
        const doPrefixItemsMatch = (prefixItems: unknown[], arrayValue: unknown[]) => {
          startIndex += prefixItems.length
          result = true
          for (let i = 0; i < prefixItems.length; i += 1) {
            const childSchema = prefixItems[i]
            if (
              typeof childSchema === 'object' &&
              childSchema != null &&
              !isMatch(defs, childSchema, arrayValue[i], options)
            ) {
              result = false
              break
            }
          }
          return result
        }
        if (!Array.isArray(value)) {
          result = false
          break
        } else if (
          'prefixItems' in schema &&
          Array.isArray(schema.prefixItems) &&
          !doPrefixItemsMatch(schema.prefixItems, value)
        ) {
          result = false
          break
        } else if ('items' in schema && schema.items === false && startIndex !== value.length) {
          result = false
          break
        } else if ('items' in schema && typeof schema.items === 'object' && schema.items != null) {
          const childSchema = schema.items
          result = true
          for (let i = startIndex; i < value.length; i += 1) {
            if (!isMatch(defs, childSchema, value[i], options)) {
              result = false
              break
            }
          }
          break
        } else {
          result = true
          break
        }
      }
      default: {
        result = false
        break
      }
    }
  } else if ('$ref' in schema) {
    const referencedSchema = lookupDef(defs, schema)
    result = referencedSchema != null && isMatch(defs, referencedSchema, value, options)
  } else if ('anyOf' in schema) {
    if (!Array.isArray(schema.anyOf)) {
      result = false
    } else {
      result = schema.anyOf.some(childSchema => {
        const childSchemaObject = objectModule.asObject(childSchema)
        return childSchemaObject != null && isMatch(defs, childSchemaObject, value, options)
      })
    }
  } else if ('allOf' in schema) {
    if (!Array.isArray(schema.allOf)) {
      result = false
    } else {
      result = schema.allOf.every(childSchema => {
        const childSchemaObject = objectModule.asObject(childSchema)
        return childSchemaObject != null && isMatch(defs, childSchemaObject, value, options)
      })
    }
  } else {
    // `enum`s are currently ignored as they are not yet used.
    result = false
  }
  return result
}
