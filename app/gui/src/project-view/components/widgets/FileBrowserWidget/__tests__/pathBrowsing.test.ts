import type { DirectoryId } from 'enso-common/src/services/Backend'
import { expect, test } from 'vitest'
import { useEnsoPaths } from '../ensoPath'
import { usePathBrowsing, type Directory } from '../pathBrowsing'
import { MOCK_FS, mockListDirectory } from './mockData'

function deriveRootId(rootPath: string): DirectoryId {
  switch (rootPath) {
    case 'enso://':
      return '0' as DirectoryId
    case 'enso://Users/user':
      return '3' as DirectoryId
    default:
      throw new Error(`Unknown rootPath: ${rootPath}`)
  }
}

function pathSegments(initialPath: string, rootPath: string): string[] {
  const rootId = deriveRootId(rootPath)
  const { parseEnsoPath } = useEnsoPaths({
    rootDirectoryId: rootId,
    rootPath,
  })
  const path = parseEnsoPath(initialPath)
  return path.ok ? path.value.segments : []
}

test.each`
  initialPath                                       | rootPath               | expectedStack           | unenteredPath
  ${''}                                             | ${'enso://'}           | ${['0']}                | ${''}
  ${''}                                             | ${'enso://Users/user'} | ${['3']}                | ${''}
  ${'enso://'}                                      | ${'enso://'}           | ${['0']}                | ${''}
  ${'enso://'}                                      | ${'enso://Users/user'} | ${['3']}                | ${''}
  ${'enso://Users/user'}                            | ${'enso://'}           | ${['0', '1', '3']}      | ${''}
  ${'enso://Users/user'}                            | ${'enso://Users/user'} | ${['3']}                | ${''}
  ${'enso://Users/user/input.csv'}                  | ${'enso://'}           | ${['0', '1', '3']}      | ${'input.csv'}
  ${'enso://Users/user/input.csv'}                  | ${'enso://Users/user'} | ${['3']}                | ${'input.csv'}
  ${'enso://Users/user/New Folder 1/input.csv'}     | ${'enso://'}           | ${['0', '1', '3', '4']} | ${'input.csv'}
  ${'enso://Users/user/New Folder 1/input.csv'}     | ${'enso://Users/user'} | ${['3', '4']}           | ${'input.csv'}
  ${'enso://Users/user/DOES_NOT_EXIST/input.csv'}   | ${'enso://'}           | ${['0', '1', '3']}      | ${'DOES_NOT_EXIST/input.csv'}
  ${'enso://Users/user/DOES_NOT_EXIST/input.csv'}   | ${'enso://Users/user'} | ${['3']}                | ${'DOES_NOT_EXIST/input.csv'}
  ${'enso://Users/user/New Folder 1/dir/input.csv'} | ${'enso://'}           | ${['0', '1', '3', '4']} | ${'dir/input.csv'}
  ${'enso://Users/user/New Folder 1/dir/input.csv'} | ${'enso://Users/user'} | ${['3', '4']}           | ${'dir/input.csv'}
`(
  'Initializing dir stack: initial path: $initialPath, root path: $rootPath',
  async ({ initialPath, rootPath, expectedStack, unenteredPath }) => {
    const segments = pathSegments(initialPath, rootPath)
    const rootId = deriveRootId(rootPath)
    const { setBrowsingPath, enteredPath, unenteredPathSuffix, currentDirectory, isPending } =
      usePathBrowsing({
        listDirectory: mockListDirectory,
      })
    expect(isPending.value).toBeTruthy()
    await setBrowsingPath({ root: rootId, segments })
    expect(isPending.value).toBeFalsy()
    expect(enteredPath.value).toEqual({
      root: rootId,
      segments: expectedStack.slice(1).map((id: string) => MOCK_FS.get(id)!.title),
    })
    expect(unenteredPathSuffix.value).toBe(unenteredPath)
    expect(currentDirectory.value?.id).toBe(expectedStack[expectedStack.length - 1])
  },
)

test('does not re-traverse unchanged prefix when entering overlapping path', async () => {
  const rootPath = 'enso://'
  const rootId = deriveRootId(rootPath)
  let calls = 0
  const countingListDirectory = async (dir: Directory) => {
    calls += 1
    return mockListDirectory(dir)
  }

  const { setBrowsingPath } = usePathBrowsing({ listDirectory: countingListDirectory })
  // First enter Users/user
  await setBrowsingPath({ root: rootId, segments: ['Users', 'user'] })
  expect(calls).toBe(2) // Users, then user

  // Extend to Users/user/New Folder 1
  await setBrowsingPath({ root: rootId, segments: ['Users', 'user', 'New Folder 1'] })
  expect(calls).toBe(3)

  // Extend with a new segment
  await setBrowsingPath({ root: rootId, segments: ['Users', 'user', 'New Folder 1', 'input.csv'] })
  expect(calls).toBe(4)

  // Reset to Users/user
  await setBrowsingPath({ root: rootId, segments: ['Users', 'user'] })
  expect(calls).toBe(4)
})
