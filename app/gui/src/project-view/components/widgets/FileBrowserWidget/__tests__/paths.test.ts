import {
  AnyAsset,
  AssetType,
  DirectoryId,
  EmailAddress,
  OrganizationId,
  OrganizationInfo,
  User,
  UserId,
} from '#/services/Backend'
import { expect, test, vi } from 'vitest'
import { useFileBrowserStack } from '../paths'

const MOCK_USER: User = {
  isEnabled: false,
  isOrganizationAdmin: false,
  rootDirectoryId: '3' as DirectoryId,
  userGroups: null,
  isEnsoTeamMember: false,
  organizationId: 'org1' as OrganizationId,
  userId: 'user' as UserId,
  name: 'user',
  email: 'doofenshmirtz@evil.com' as EmailAddress,
}
const MOCK_ORGANIZATION_INFO: OrganizationInfo = {
  id: 'org1' as OrganizationId,
  name: 'Doofenshmirtz Evil Inc.',
  email: 'doofenshmirtz@evil.com' as EmailAddress,
  website: null,
  address: null,
  picture: null,
  subscription: null,
}
const MOCK_FS: Record<string, { type: AnyAsset['type']; name: string; contents: string[] }> = {
  '0': {
    type: AssetType.directory,
    name: '',
    contents: ['1', '2'],
  },
  '1': {
    type: AssetType.directory,
    name: 'Users',
    contents: ['3'],
  },
  '2': {
    type: AssetType.directory,
    name: 'Teams',
    contents: [],
  },
  '3': {
    type: AssetType.directory,
    name: 'user',
    contents: ['4', '5'],
  },
  '4': {
    type: AssetType.directory,
    name: 'New Folder 1',
    contents: ['6', '7'],
  },
  '5': {
    type: AssetType.file,
    name: 'input.csv',
    contents: [],
  },
  '6': {
    type: AssetType.file,
    name: 'input.csv',
    contents: [],
  },
  '7': {
    type: AssetType.directory,
    name: 'Nested',
    contents: ['8'],
  },
  '8': {
    type: AssetType.file,
    name: 'test.txt',
    contents: [],
  },
}
function fixture(
  rootPath: string,
  rootDirectoryId: string,
  initialPath: string,
  writeMode: boolean,
) {
  return useFileBrowserStack(
    {
      rootPath: vi.fn(() => rootPath),
      rootDirectoryId: vi.fn(() => rootDirectoryId as DirectoryId),
    },
    initialPath,
    MOCK_USER,
    writeMode,
    (dir) => {
      expect(MOCK_FS).toHaveProperty(dir.id)
      return Promise.resolve(
        MOCK_FS[dir.id]!.contents.map((id) => {
          expect(MOCK_FS).toHaveProperty(id)
          const asset = MOCK_FS[id]!
          return {
            type: asset.type,
            id: id,
            title: asset.name,
            modifiedAt: '',
            parentId: dir.id,
            permissions: null,
            labels: null,
            description: null,
            projectState: null,
            extension: null,
            parentsPath: '',
            virtualParentsPath: '',
          } as any // The type system of assets is cruel.
        }),
      )
    },
  )
}

test.each`
  initialPath                                       | rootPath               | rootId | expectedStack
  ${''}                                             | ${'enso://'}           | ${'0'} | ${['0', '1', '3']}
  ${''}                                             | ${'enso://Users/user'} | ${'3'} | ${['3']}
  ${'enso://'}                                      | ${'enso://'}           | ${'0'} | ${['0']}
  ${'enso://'}                                      | ${'enso://Users/user'} | ${'3'} | ${['3']}
  ${'enso://Users/user'}                            | ${'enso://'}           | ${'0'} | ${['0', '1', '3']}
  ${'enso://Users/user'}                            | ${'enso://Users/user'} | ${'3'} | ${['3']}
  ${'enso://Users/user/input.csv'}                  | ${'enso://'}           | ${'0'} | ${['0', '1', '3']}
  ${'enso://Users/user/input.csv'}                  | ${'enso://Users/user'} | ${'3'} | ${['3']}
  ${'enso://Users/user/New Folder 1/input.csv'}     | ${'enso://'}           | ${'0'} | ${['0', '1', '3', '4']}
  ${'enso://Users/user/New Folder 1/input.csv'}     | ${'enso://Users/user'} | ${'3'} | ${['3', '4']}
  ${'enso://Users/user/New Folder 2/input.csv'}     | ${'enso://'}           | ${'0'} | ${['0', '1', '3']}
  ${'enso://Users/user/New Folder 2/input.csv'}     | ${'enso://Users/user'} | ${'3'} | ${['3']}
  ${'enso://Users/user/New Folder 1/dir/input.csv'} | ${'enso://'}           | ${'0'} | ${['0', '1', '3', '4']}
  ${'enso://Users/user/New Folder 1/dir/input.csv'} | ${'enso://Users/user'} | ${'3'} | ${['3', '4']}
`(
  'Initializing dir stack: initial path: $initialPath, root path: $rootPath',
  async ({ initialPath, rootPath, rootId, expectedStack }) => {
    const { directoryStack, isDirectoryStackInitializing, initializeStack } = fixture(
      rootPath,
      rootId,
      initialPath,
      false,
    )
    expect(isDirectoryStackInitializing.value).toBeTruthy()
    const result = await initializeStack(MOCK_USER, MOCK_ORGANIZATION_INFO)
    expect(isDirectoryStackInitializing.value).toBeFalsy()
    expect(result).toEqual({ ok: true, value: undefined })
    const expectedStackStructure = expectedStack.map((id: string, index: number) =>
      expect.objectContaining({
        id: id as DirectoryId,
        title: index === 0 ? 'Cloud' : MOCK_FS[id]?.name,
      }),
    )
    expect(directoryStack.value).toEqual(expectedStackStructure)
  },
)

test.each`
  initialPath                                       | expectedInputContents
  ${''}                                             | ${''}
  ${''}                                             | ${''}
  ${'enso://Users/user/input.csv'}                  | ${'input.csv'}
  ${'enso://Users/user/input 2.csv'}                | ${'input 2.csv'}
  ${'enso://Users/user/New Folder 1/input.csv'}     | ${'input.csv'}
  ${'enso://Users/user/New Folder 1/dir/input.csv'} | ${''}
`(
  'Initial input content in write mode $initialPath',
  async ({ initialPath, expectedInputContents }) => {
    const { initializeStack, filenameInputContents, highlightedName } = fixture(
      'enso://',
      '0',
      initialPath,
      true,
    )
    await initializeStack(MOCK_USER, MOCK_ORGANIZATION_INFO)
    expect(filenameInputContents.value).toBe(expectedInputContents)
    expect(highlightedName.value).toBe(expectedInputContents)
  },
)

test.each`
  initialPath                                   | rootPath               | rootDirId | checks
  ${''}                                         | ${'enso://'}           | ${'0'}    | ${[[[], false], [['1', '3'], false]]}
  ${''}                                         | ${'enso://Users/user'} | ${'3'}    | ${[[[], false], [['4'], false]]}
  ${'enso://Users/user/input.csv'}              | ${'enso://'}           | ${'0'}    | ${[[[], false], [['1', '3'], true], [['1', '3', '4'], false]]}
  ${'enso://Users/user/input.csv'}              | ${'enso://Users/user'} | ${'3'}    | ${[[[], true], [['4'], false]]}
  ${'enso://Users/user/New Folder 1/input.csv'} | ${'enso://'}           | ${'0'}    | ${[[[], false], [['1', '3'], false], [['1', '3', '4'], true]]}
  ${'enso://Users/user/New Folder 1/input.csv'} | ${'enso://Users/user'} | ${'3'}    | ${[[[], false], [['4'], true]]}
`(
  'Highlight entry in read mode (path: $initialPath, root: $rootPath)',
  async ({ initialPath, rootPath, rootDirID, checks }) => {
    const { directoryStack, highlightedName } = fixture(rootPath, rootDirID, initialPath, false)
    for (const [stack, expected] of checks) {
      directoryStack.value = [
        { id: rootDirID, title: 'Cloud' },
        ...stack.map((id: string) => ({ id, title: MOCK_FS[id]?.name })),
      ]
      expect(highlightedName.value, `Wrong highlight in [${stack.join(',')}] stack`).toBe(
        expected ? 'input.csv' : undefined,
      )
    }
  },
)

test.each`
  initialInputContents                  | expectedStack                | expectedInputContents
  ${''}                                 | ${['0', '1', '3']}           | ${''}
  ${'New Folder 1'}                     | ${['0', '1', '3']}           | ${'New Folder 1'}
  ${'New Folder 1/input.csv'}           | ${['0', '1', '3', '4']}      | ${'input.csv'}
  ${'New Folder 1/Nested/test.txt'}     | ${['0', '1', '3', '4', '7']} | ${'test.txt'}
  ${'New Folder 1/Nested/non-existent'} | ${['0', '1', '3', '4', '7']} | ${'non-existent'}
  ${'New Folder 1/invalid/test.txt'}    | ${['0', '1', '3', '4']}      | ${'invalid/test.txt'}
`(
  'Enter subdirectories (input: $initialInputContents)',
  async ({ initialInputContents, expectedStack, expectedInputContents }) => {
    const { initializeStack, filenameInputContents, enterSubdirectories, directoryStack } = fixture(
      'enso://',
      '0',
      '',
      true,
    )
    await initializeStack(MOCK_USER, MOCK_ORGANIZATION_INFO)
    filenameInputContents.value = initialInputContents
    await enterSubdirectories()
    const expectedStackStructure = expectedStack.map((id: string, index: number) =>
      expect.objectContaining({
        id: id as DirectoryId,
        title: index === 0 ? 'Cloud' : MOCK_FS[id]?.name,
      }),
    )
    expect(directoryStack.value).toEqual(expectedStackStructure)
    expect(filenameInputContents.value).toBe(expectedInputContents)
  },
)

test.each`
  initialPath                         | inputContents     | expectedResult
  ${''}                               | ${''}             | ${{ exists: false }}
  ${''}                               | ${'input.csv'}    | ${{ exists: true, type: AssetType.file }}
  ${'enso://Users/user/New Folder 1'} | ${'input.csv'}    | ${{ exists: true, type: AssetType.file }}
  ${'enso://Users/user/New Folder 1'} | ${'non-existent'} | ${{ exists: false }}
  ${'enso://Users/user/New Folder 1'} | ${'Nested'}       | ${{ exists: true, type: AssetType.directory }}
`(
  'File exists (path: $initialPath, input: $inputContents)',
  async ({ initialPath, inputContents, expectedResult }) => {
    const { initializeStack, assetExists } = fixture('enso://', '0', initialPath, true)
    await initializeStack(MOCK_USER, MOCK_ORGANIZATION_INFO)
    const exists = await assetExists(inputContents)
    expect(exists).toEqual(expectedResult)
  },
)
