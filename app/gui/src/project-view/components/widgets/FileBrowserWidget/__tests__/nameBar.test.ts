import { useNameBar } from '@/components/widgets/FileBrowserWidget/nameBar'
import { expect, test } from 'vitest'

test.each`
  fullFilename         | expectedFilename | expectedExtension
  ${''}                | ${''}            | ${''}
  ${'test.txt'}        | ${'test'}        | ${'txt'}
  ${'test'}            | ${'test'}        | ${''}
  ${'folder/test.txt'} | ${'folder/test'} | ${'txt'}
`('setFilename $fullFilename', ({ fullFilename, expectedFilename, expectedExtension }) => {
  const { fullFilePath, filenameInput, extensionInput } = useNameBar()
  filenameInput.value = expectedFilename
  extensionInput.value = expectedExtension
  expect(fullFilePath.value).toBe(fullFilename)
})

test.each`
  filename      | extension | expected
  ${'test'}     | ${'txt'}  | ${'test.txt'}
  ${'test'}     | ${''}     | ${'test'}
  ${'test.txt'} | ${'xml'}  | ${'test.txt'}
  ${''}         | ${'txt'}  | ${''}
`('fullFilePath $filename $extension', ({ filename, extension, expected }) => {
  const { fullFilePath, filenameInput, extensionInput } = useNameBar()
  filenameInput.value = filename
  extensionInput.value = extension
  expect(fullFilePath.value).toBe(expected)
})
