import { expect, test } from 'vitest'
import { normalizeName } from '../nameValidation'

test('should sanitize the name of the project', () => {
  expect(normalizeName('My_Project')).toBe('My_Project')
  expect(normalizeName('My___Project')).toBe('My___Project')
  expect(normalizeName('myProject')).toBe('MyProject')
  expect(normalizeName('myPro??^ject123')).toBe('MyProject123')
  expect(normalizeName('???%$6543lib')).toBe('Project_6543lib')
  expect(normalizeName('MyProject™')).toBe('MyProject')
  expect(normalizeName('$$$$')).toBe('Project')
  expect(normalizeName('$$42$$')).toBe('Project_42')
  expect(normalizeName('AoC_1')).toBe('AoC_1')
  expect(normalizeName('🚀')).toBe('Project')
  expect(normalizeName('🚀_😊')).toBe('Project')
  expect(normalizeName('🚀_😊__')).toBe('Project')
  expect(normalizeName('__🚀_😊_')).toBe('Project')
  expect(normalizeName('')).toBe('Project')
  expect(normalizeName('123')).toBe('Project_123')
  expect(normalizeName('Test123')).toBe('Test123')
  expect(normalizeName('test_123')).toBe('Test_123')
})
