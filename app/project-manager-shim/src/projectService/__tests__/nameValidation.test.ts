import { expect, test } from 'vitest'
import { normalizedName, validateName } from '../nameValidation'

test('normalizedName should sanitize the name of the project', () => {
  expect(normalizedName('My_Project')).toBe('My_Project')
  expect(normalizedName('My___Project')).toBe('My___Project')
  expect(normalizedName('myProject')).toBe('MyProject')
  expect(normalizedName('myPro??^ject123')).toBe('MyProject123')
  expect(normalizedName('???%$6543lib')).toBe('Project_6543lib')
  expect(normalizedName('MyProject™')).toBe('MyProject')
  expect(normalizedName('$$$$')).toBe('Project')
  expect(normalizedName('$$42$$')).toBe('Project_42')
  expect(normalizedName('AoC_1')).toBe('AoC_1')
  expect(normalizedName('🚀')).toBe('Project')
  expect(normalizedName('🚀_😊')).toBe('Project')
  expect(normalizedName('🚀_😊__')).toBe('Project')
  expect(normalizedName('__🚀_😊_')).toBe('Project')
  expect(normalizedName('')).toBe('Project')
  expect(normalizedName('123')).toBe('Project_123')
  expect(normalizedName('Test123')).toBe('Test123')
  expect(normalizedName('test_123')).toBe('Test_123')
})

test('validateName should accept valid project names', () => {
  expect(validateName('MyProject')).toBe('MyProject')
  expect(validateName('Project')).toBe('Project')
  expect(validateName('Test123')).toBe('Test123')
  expect(validateName('My_Project_2')).toBe('My_Project_2')
  expect(validateName('AoC_1')).toBe('AoC_1')
  expect(validateName('PROJECT')).toBe('PROJECT')
  expect(validateName('P')).toBe('P')
  expect(validateName('Project_123')).toBe('Project_123')
})

test('validateName should reject empty names', () => {
  const result = validateName('')
  expect(result).toEqual({ type: 'Empty' })
})

test('validateName should reject names not starting with capital letter', () => {
  expect(validateName('myProject')).toEqual({ type: 'ShouldStartWithCapitalLetter' })
  expect(validateName('project')).toEqual({ type: 'ShouldStartWithCapitalLetter' })
  expect(validateName('123Project')).toEqual({ type: 'ShouldStartWithCapitalLetter' })
  expect(validateName('_Project')).toEqual({ type: 'ShouldStartWithCapitalLetter' })
  expect(validateName('1Project')).toEqual({ type: 'ShouldStartWithCapitalLetter' })
})

test('validateName should reject names with invalid characters', () => {
  const result1 = validateName('My-Project')
  expect(result1).toEqual({
    type: 'ContainsInvalidCharacters',
    characters: new Set(['-']),
  })

  const result2 = validateName('My Project')
  expect(result2).toEqual({
    type: 'ContainsInvalidCharacters',
    characters: new Set([' ']),
  })

  const result3 = validateName('MyProject!')
  expect(result3).toEqual({
    type: 'ContainsInvalidCharacters',
    characters: new Set(['!']),
  })

  const result4 = validateName('My.Project')
  expect(result4).toEqual({
    type: 'ContainsInvalidCharacters',
    characters: new Set(['.']),
  })

  const result5 = validateName('MyProject™')
  expect(result5).toEqual({
    type: 'ContainsInvalidCharacters',
    characters: new Set(['™']),
  })

  const result6 = validateName('Project@#$')
  expect(result6).toEqual({
    type: 'ContainsInvalidCharacters',
    characters: new Set(['@', '#', '$']),
  })
})
