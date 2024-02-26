import * as v from 'vitest'

import SCHEMA from '#/data/dataLinkSchema.json' assert { type: 'json' }
import * as jsonSchema from '#/utilities/jsonSchema'

v.test('adds 1 + 2 to equal 3', () => {
    // jsonSchema.isMatch()
    v.expect(1+2).toBe(4)
})