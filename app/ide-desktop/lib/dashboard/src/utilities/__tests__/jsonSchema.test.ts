/** @file Tests for JSON schema utility functions. */
import * as fc from '@fast-check/vitest'
import * as v from 'vitest'

import * as jsonSchema from '#/utilities/jsonSchema'

// =============
// === Tests ===
// =============

fc.test.prop({
  value: fc.fc.anything(),
})('converting between constant value and schema', ({ value }) => {
  const schema = jsonSchema.constantValueToSchema(value)
  if (schema != null) {
    const extractedValue = jsonSchema.constantValue({}, schema)[0]
    v.expect(
      extractedValue,
      `\`${JSON.stringify(value)}\` should round trip to schema and back`
    ).toEqual(value)
    v.expect(
      jsonSchema.isMatch({}, schema, value),
      `\`${JSON.stringify(value)}\` should match its converted schema`
    ).toBe(true)
  }
})
