/** @file Tests for JSON schema utility functions. */
import * as fc from '@fast-check/vitest'
import Ajv from 'ajv/dist/2020'
import * as v from 'vitest'

import * as jsonSchema from '#/utilities/jsonSchema'

// =================
// === Constants ===
// =================

const AJV = new Ajv()

// =============
// === Tests ===
// =============

fc.test.prop({
  value: fc.fc.anything({ withNullPrototype: true }),
})('converting between constant value and schema', ({ value }) => {
  const schema = jsonSchema.constantValueToSchema(value)
  if (schema != null) {
    const extractedValue = jsonSchema.constantValueOfSchema({}, schema)[0]
    v.expect(
      extractedValue,
      `\`${JSON.stringify(value)}\` should round trip to schema and back`,
    ).toEqual(value)
    v.expect(
      AJV.validate(schema, value),
      `\`${JSON.stringify(value)}\` should match its converted schema`,
    ).toBe(true)
  }
})

// eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
v.test.each([{ value: JSON.parse('{"__proto__":{}}') }])(
  'converting between constant value and schema',
  ({ value }) => {
    const schema = jsonSchema.constantValueToSchema(value)
    if (schema != null) {
      const extractedValue = jsonSchema.constantValueOfSchema({}, schema)[0]
      v.expect(
        extractedValue,
        `\`${JSON.stringify(value)}\` should round trip to schema and back`,
      ).toEqual(value)
      v.expect(
        AJV.validate(schema, value),
        `\`${JSON.stringify(value)}\` should match its converted schema`,
      ).toBe(true)
    }
  },
)

const STRING_SCHEMA = { type: 'string' } as const
fc.test.prop({ value: fc.fc.string() })('string schema', ({ value }) => {
  const constSchema = { const: value, type: 'string' }
  v.expect(AJV.validate(STRING_SCHEMA, value)).toBe(true)
  v.expect(AJV.validate(constSchema, value)).toBe(true)
  v.expect(jsonSchema.constantValueOfSchema({}, constSchema)[0]).toBe(value)
})

const NUMBER_SCHEMA = { type: 'number' } as const
fc.test.prop({ value: fc.fc.float() })('number schema', ({ value }) => {
  if (Number.isFinite(value)) {
    const constSchema = { const: value, type: 'number' }
    v.expect(AJV.validate(NUMBER_SCHEMA, value)).toBe(true)
    v.expect(AJV.validate(constSchema, value)).toBe(true)
    v.expect(jsonSchema.constantValueOfSchema({}, constSchema)[0]).toBe(value)
  }
})

fc.test.prop({
  value: fc.fc.float().filter((n) => n > 0),
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  multiplier: fc.fc.integer({ min: -1_000_000, max: 1_000_000 }),
})('number multiples', ({ value, multiplier }) => {
  const schema = { type: 'number', multipleOf: value }
  if (Number.isFinite(value)) {
    v.expect(AJV.validate(schema, 0)).toBe(true)
    v.expect(AJV.validate(schema, value)).toBe(true)
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    if (Math.abs(value * (multiplier + 0.5)) < Number.MAX_SAFE_INTEGER) {
      v.expect(AJV.validate(schema, value * multiplier)).toBe(true)
      if (value !== 0) {
        // eslint-disable-next-line @typescript-eslint/no-magic-numbers
        v.expect(AJV.validate(schema, value * (multiplier + 0.5))).toBe(false)
      }
    }
  }
})

const INTEGER_SCHEMA = { type: 'integer' } as const
fc.test.prop({ value: fc.fc.integer() })('integer schema', ({ value }) => {
  const constSchema = { const: value, type: 'integer' }
  v.expect(AJV.validate(INTEGER_SCHEMA, value)).toBe(true)
  v.expect(AJV.validate(constSchema, value)).toBe(true)
  v.expect(jsonSchema.constantValueOfSchema({}, constSchema)[0]).toBe(value)
})

fc.test.prop({
  value: fc.fc.integer().filter((n) => n > 0),
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  multiplier: fc.fc.integer({ min: -1_000_000, max: 1_000_000 }),
})('integer multiples', ({ value, multiplier }) => {
  const schema = { type: 'integer', multipleOf: value }
  v.expect(AJV.validate(schema, 0)).toBe(true)
  v.expect(AJV.validate(schema, value)).toBe(true)
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  if (Math.abs(value * (multiplier + 0.5)) < Number.MAX_SAFE_INTEGER) {
    v.expect(AJV.validate(schema, value * multiplier)).toBe(true)
    if (value !== 0) {
      // eslint-disable-next-line @typescript-eslint/no-magic-numbers
      v.expect(AJV.validate(schema, value * (multiplier + 0.5))).toBe(false)
    }
  }
})
