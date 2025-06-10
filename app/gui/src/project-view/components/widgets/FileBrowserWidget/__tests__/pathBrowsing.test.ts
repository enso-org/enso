import { type DirectoryId } from '#/services/Backend'
import { expect, test } from 'vitest'
import { useEnsoPaths } from '../ensoPath'
import { usePathBrowsing } from '../pathBrowsing'
import { MOCK_FS, mockListDirectory } from './mockData'

function pathSegments(initialPath: string, rootPath: string, rootId: DirectoryId): string[] {
  const { parseEnsoPath } = useEnsoPaths({
    rootDirectoryId: rootId,
    rootPath,
  })
  const path = parseEnsoPath(initialPath)
  return path.ok ? path.value.segments : ['Users', 'user']
}

test.each`
  initialPath                                       | rootPath               | rootId | expectedStack           | unenteredPath
  ${''}                                             | ${'enso://'}           | ${'0'} | ${['0', '1', '3']}      | ${''}
  ${''}                                             | ${'enso://Users/user'} | ${'3'} | ${['3']}                | ${''}
  ${'enso://'}                                      | ${'enso://'}           | ${'0'} | ${['0']}                | ${''}
  ${'enso://'}                                      | ${'enso://Users/user'} | ${'3'} | ${['3']}                | ${''}
  ${'enso://Users/user'}                            | ${'enso://'}           | ${'0'} | ${['0', '1', '3']}      | ${''}
  ${'enso://Users/user'}                            | ${'enso://Users/user'} | ${'3'} | ${['3']}                | ${''}
  ${'enso://Users/user/input.csv'}                  | ${'enso://'}           | ${'0'} | ${['0', '1', '3']}      | ${'input.csv'}
  ${'enso://Users/user/input.csv'}                  | ${'enso://Users/user'} | ${'3'} | ${['3']}                | ${'input.csv'}
  ${'enso://Users/user/New Folder 1/input.csv'}     | ${'enso://'}           | ${'0'} | ${['0', '1', '3', '4']} | ${'input.csv'}
  ${'enso://Users/user/New Folder 1/input.csv'}     | ${'enso://Users/user'} | ${'3'} | ${['3', '4']}           | ${'input.csv'}
  ${'enso://Users/user/New Folder 2/input.csv'}     | ${'enso://'}           | ${'0'} | ${['0', '1', '3']}      | ${''}
  ${'enso://Users/user/New Folder 2/input.csv'}     | ${'enso://Users/user'} | ${'3'} | ${['3']}                | ${''}
  ${'enso://Users/user/New Folder 1/dir/input.csv'} | ${'enso://'}           | ${'0'} | ${['0', '1', '3', '4']} | ${''}
  ${'enso://Users/user/New Folder 1/dir/input.csv'} | ${'enso://Users/user'} | ${'3'} | ${['3', '4']}           | ${''}
`(
  'Initializing dir stack: initial path: $initialPath, root path: $rootPath',
  async ({ initialPath, rootPath, rootId, expectedStack, unenteredPath }) => {
    const segments = pathSegments(initialPath, rootPath, rootId)
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
