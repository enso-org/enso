/** @file Tests for `error.ts`. */
import * as fc from '@fast-check/vitest'
import * as v from 'vitest'

import * as arrayModule from '../array'

// =============
// === Tests ===
// =============

fc.test.prop({ array: fc.fc.array(fc.fc.anything()) })('`array.shallowEqual`', ({ array }) => {
  v.expect(arrayModule.shallowEqual(array, [...array]))
})

fc.test.prop({
  array: fc.fc.array(fc.fc.anything(), { minLength: 1 }).chain(array =>
    fc.fc.record({
      array: fc.fc.constant(array),
      i: fc.fc.nat(array.length - 1),
    })
  ),
})('`array.includesPredicate`', ({ array: { array, i } }) => {
  v.expect(
    arrayModule.includesPredicate(array)(array[i]),
    `'${JSON.stringify(array)}' should include '${JSON.stringify(array[i])}'`
  ).toBe(true)
  v.expect(arrayModule.includesPredicate(array)({}), 'unique object should not be in array').toBe(
    false
  )
})
