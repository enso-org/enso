import { tryGetSoleValue } from 'shared/util/data/iterable'
import { expect, test } from 'vitest'

test('tryGetSoleValue', () => {
  expect(tryGetSoleValue([])).toBeUndefined()
  expect(tryGetSoleValue([1])).toEqual(1)
  expect(tryGetSoleValue([1, 2])).toBeUndefined()
})
