import { DirectoryId } from '#/services/Backend'
import { uuidv4 } from 'lib0/random.js'
import { expect, test } from 'vitest'
import { useEnsoPaths } from '../ensoPath'

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
