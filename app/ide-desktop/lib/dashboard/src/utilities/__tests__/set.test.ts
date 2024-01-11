/** @file Tests for `error.ts`. */
import * as fc from '@fast-check/vitest'
import * as v from 'vitest'

import * as setModule from '../set'

// =============
// === Tests ===
// =============

fc.test.prop({
    set: fc.fc.array(fc.fc.anything(), { minLength: 1 }).map(array => new Set(array)),
    item: fc.fc.anything(),
})('Manipulating `Set` presence', ({ set, item }) => {
    v.expect(setModule.withPresence(set, item, true).has(item)).toBe(true)
    v.expect(setModule.withPresence(set, item, false).has(item)).toBe(false)
    v.expect(setModule.setPresence(set, item, true).has(item)).toBe(true)
    v.expect(setModule.setPresence(set, item, false).has(item)).toBe(false)
})
