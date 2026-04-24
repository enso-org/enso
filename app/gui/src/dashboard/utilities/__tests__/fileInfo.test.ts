/** @file Tests for `fileInfo.ts`. */
import { fileExtension } from 'enso-common/src/utilities/file'
import * as v from 'vitest'

v.test('fileExtension', () => {
  v.expect(fileExtension('image.png')).toBe('png')
  v.expect(fileExtension('.gif')).toBe('gif')
  v.expect(fileExtension('fileInfo.spec.js')).toBe('js')
})
