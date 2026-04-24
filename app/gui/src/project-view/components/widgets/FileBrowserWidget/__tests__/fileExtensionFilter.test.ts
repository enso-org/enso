import { AssetType } from 'enso-common/src/services/Backend'
import { expect, test } from 'vitest'
import { ref } from 'vue'
import { useFileExtensionFilter } from '../fileExtensionFilter'

function mockFile(title: string) {
  return {
    type: AssetType.file,
    title,
  }
}

function mockDirectory(title: string) {
  return {
    type: AssetType.directory,
    title,
  }
}

test('Displayed extension and filename suffix', () => {
  const inputContents = ref('')
  const fileExtensionInputContents = ref('')
  const { displayedExtension, filenameSuffix, filter } = useFileExtensionFilter(
    inputContents,
    fileExtensionInputContents,
  )

  expect(displayedExtension.value).toBe('*')
  expect(filenameSuffix.value).toBe('')

  fileExtensionInputContents.value = 'txt'
  expect(displayedExtension.value).toBe('txt')
  expect(filenameSuffix.value).toBe('.txt')

  filter.value = {
    type: 'predefined',
    label: 'Excel',
    extensions: ['xlsx', 'xls'],
  }
  expect(displayedExtension.value).toBe('Excel')
  expect(filenameSuffix.value).toBe('.xlsx')

  inputContents.value = 'test.txt'
  expect(displayedExtension.value).toBe('Excel')
  expect(filenameSuffix.value).toBe('')
})

test('Filtering', () => {
  const inputContents = ref('')
  const fileExtensionInputContents = ref('')
  const { matches, filter } = useFileExtensionFilter(inputContents, fileExtensionInputContents)

  expect(matches(mockFile('test.txt'))).toBe(true)
  expect(matches(mockFile('test.txt.backup'))).toBe(true)
  expect(matches(mockFile('test.png'))).toBe(true)
  expect(matches(mockDirectory('test'))).toBe(true)
  expect(matches(mockDirectory('test.txt'))).toBe(true)

  fileExtensionInputContents.value = 'txt'
  expect(matches(mockFile('test.txt'))).toBe(true)
  expect(matches(mockFile('test.txt.backup'))).toBe(false)
  expect(matches(mockFile('test.png'))).toBe(false)
  expect(matches(mockDirectory('test'))).toBe(true)
  expect(matches(mockDirectory('test.txt'))).toBe(true)

  fileExtensionInputContents.value = '*'
  expect(matches(mockFile('test.txt'))).toBe(true)
  expect(matches(mockFile('test.txt.backup'))).toBe(true)
  expect(matches(mockFile('test.png'))).toBe(true)
  expect(matches(mockDirectory('test'))).toBe(true)
  expect(matches(mockDirectory('test.txt'))).toBe(true)

  filter.value = {
    type: 'predefined',
    label: 'Some files',
    extensions: ['txt', 'png'],
  }
  expect(matches(mockFile('test.txt'))).toBe(true)
  expect(matches(mockFile('test.txt.backup'))).toBe(false)
  expect(matches(mockFile('test.png'))).toBe(true)
  expect(matches(mockDirectory('test'))).toBe(true)
  expect(matches(mockDirectory('test.txt'))).toBe(true)
})
