import { expect, test } from 'vitest'
import { tryGetSoleValue } from '../iterable'

test('tryGetSoleValue', () => {
  expect(tryGetSoleValue([])).toBeUndefined()
  expect(tryGetSoleValue([1])).toEqual(1)
  expect(tryGetSoleValue([1, 2])).toBeUndefined()
})
