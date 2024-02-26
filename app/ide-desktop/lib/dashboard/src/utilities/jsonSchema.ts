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
        if (!partial && (!('items' in schema) || schema.items !== false)) {
          // This array may contain extra items.
          result = []
          break
        } else if (!('prefixItems' in schema) || !Array.isArray(schema.prefixItems)) {
          result = [[]]
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
