import { DirectoryId } from 'enso-common/src/services/Backend'
import { uuidv4 } from 'lib0/random.js'
import { describe, expect, test } from 'vitest'
import { pathToSegments, useEnsoPaths } from '../ensoPath'

test.each`
  userRootPath           | fromUserRoot | segments           | expected
  ${'enso://'}           | ${true}      | ${[]}              | ${'enso://'}
  ${'enso://Users/user'} | ${true}      | ${[]}              | ${'enso://Users/user'}
  ${'enso://Users/user'} | ${false}     | ${[]}              | ${'enso://'}
  ${'enso://'}           | ${true}      | ${['seg']}         | ${'enso://seg'}
  ${'enso://Users/user'} | ${true}      | ${['seg']}         | ${'enso://Users/user/seg'}
  ${'enso://Users/user'} | ${false}     | ${['seg']}         | ${'enso://seg'}
  ${'enso://'}           | ${true}      | ${['seg', 'seg2']} | ${'enso://seg/seg2'}
  ${'enso://Users/user'} | ${true}      | ${['seg', 'seg2']} | ${'enso://Users/user/seg/seg2'}
  ${'enso://Users/user'} | ${false}     | ${['seg', 'seg2']} | ${'enso://seg/seg2'}
`(
  'Printing ensopath $expected with user root $userRootPath',
  ({ userRootPath, fromUserRoot, segments, expected }) => {
    const files = {
      rootDirectoryId: DirectoryId(`directory-${uuidv4()}`),
      rootPath: userRootPath,
    }
    const path = {
      root: fromUserRoot ? files.rootDirectoryId : DirectoryId(`directory-${uuidv4()}`),
      segments,
    }
    const paths = useEnsoPaths(files)
    expect(paths.printEnsoPath(path)).toBe(expected)
  },
)

describe('pathToSegments', () => {
  test('rejects non-enso scheme', () => {
    const res = pathToSegments('file:///tmp')
    expect(res.ok).toBe(false)
  })

  test('parses empty enso root', () => {
    const res = pathToSegments('enso://')
    expect(res.ok).toBe(true)
    expect(res.ok && res.value).toEqual([])
  })

  test('parses segments without trailing slash', () => {
    const res = pathToSegments('enso://a/b/c')
    expect(res.ok).toBe(true)
    expect(res.ok && res.value).toEqual(['a', 'b', 'c'])
  })

  test('keeps empty last segment for trailing slash', () => {
    const res = pathToSegments('enso://a/b/c/')
    expect(res.ok).toBe(true)
    expect(res.ok && res.value).toEqual(['a', 'b', 'c', ''])
  })
})

describe('useEnsoPaths.parseEnsoPath', () => {
  test.each`
    userRootPath           | inputPath                        | expectedSegments
    ${'enso://'}           | ${'enso://Users/user/input.csv'} | ${['Users', 'user', 'input.csv']}
    ${'enso://Users/user'} | ${'enso://Users/user/input.csv'} | ${['input.csv']}
    ${'enso://Users/user'} | ${'enso://Other/dir/file.txt'}   | ${[]}
  `(
    'parses $inputPath relative to $userRootPath',
    ({ userRootPath, inputPath, expectedSegments }) => {
      const files = {
        rootDirectoryId: DirectoryId(`directory-${uuidv4()}`),
        rootPath: userRootPath as string,
      }
      const { parseEnsoPath } = useEnsoPaths(files)
      const res = parseEnsoPath(inputPath as string)
      expect(res.ok).toBe(true)
      expect(res.ok && res.value.segments).toEqual(expectedSegments)
    },
  )

  test('parse rejects invalid scheme', () => {
    const files = {
      rootDirectoryId: DirectoryId(`directory-${uuidv4()}`),
      rootPath: 'enso://',
    }
    const { parseEnsoPath } = useEnsoPaths(files)
    const res = parseEnsoPath('http://example.com')
    expect(res.ok).toBe(false)
  })
})
