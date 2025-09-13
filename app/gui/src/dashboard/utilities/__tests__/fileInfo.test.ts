/** @file Tests for `fileInfo.ts`. */
import * as v from 'vitest'

import * as fileInfo from 'enso-common/src/utilities/file'

v.test('fileExtension', () => {
  v.expect(fileInfo.fileExtension('image.png')).toBe('png')
  v.expect(fileInfo.fileExtension('.gif')).toBe('gif')
  v.expect(fileInfo.fileExtension('fileInfo.spec.js')).toBe('js')
})
