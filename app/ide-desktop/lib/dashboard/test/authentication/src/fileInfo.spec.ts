/** @file Tests for `fileInfo.ts`. */
import * as test from '@playwright/test'

import * as fileInfo from '../../../src/authentication/src/fileInfo'

/* eslint-disable @typescript-eslint/no-magic-numbers */

test.test('fileExtension', () => {
    test.expect(fileInfo.fileExtension('image.png')).toBe('png')
    test.expect(fileInfo.fileExtension('.gif')).toBe('gif')
    test.expect(fileInfo.fileExtension('fileInfo.spec.js')).toBe('js')
})
