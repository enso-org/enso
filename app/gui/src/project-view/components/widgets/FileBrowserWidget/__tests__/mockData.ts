import type { AnyAsset, AssetType, ListDirectoryResponseBody } from '#/services/Backend'
import type { Directory } from '@/components/widgets/FileBrowserWidget/pathBrowsing'

interface MockAssetSpec {
  type: AssetType
  title: string
  contents: string[]
}
const ASSET_SPECS = new Map([
  [
    '0',
    {
      type: 'directory' as const,
      title: '',
      contents: ['1', '2'],
    },
  ],
  [
    '1',
    {
      type: 'directory' as const,
      title: 'Users',
      contents: ['3'],
    },
  ],
  [
    '2',
    {
      type: 'directory' as const,
      title: 'Teams',
      contents: [],
    },
  ],
  [
    '3',
    {
      type: 'directory' as const,
      title: 'user',
      contents: ['4', '5'],
    },
  ],
  [
    '4',
    {
      type: 'directory' as const,
      title: 'New Folder 1',
      contents: ['6', '7'],
    },
  ],
  [
    '5',
    {
      type: 'file' as const,
      title: 'input.csv',
      contents: [],
    },
  ],
  [
    '6',
    {
      type: 'file' as const,
      title: 'input.csv',
      contents: [],
    },
  ],
  [
    '7',
    {
      type: 'directory' as const,
      title: 'Nested',
      contents: ['8'],
    },
  ],
  [
    '8',
    {
      type: 'file' as const,
      title: 'test.txt',
      contents: [],
    },
  ],
])
function buildAssetTree(assetSpecs: Map<string, MockAssetSpec>) {
  const assets = new Map()
  const parents = new Map()
  for (const [dirId, asset] of assetSpecs.entries())
    for (const childId of asset.contents) parents.set(childId, dirId)
  for (const [id, { type, title }] of assetSpecs.entries()) {
    assets.set(
      id,
      {
        type,
        id,
        title,
        modifiedAt: '',
        parentId: parents.get(id)!,
        permissions: null,
        labels: null,
        description: null,
        projectState: null,
        extension: null,
        parentsPath: '',
        virtualParentsPath: '',
      } as any /* The type system of assets is cruel. */,
    )
  }
  return assets
}
export const MOCK_FS: Map<string, AnyAsset> = buildAssetTree(ASSET_SPECS)

/** Mock implementation of `listDirectory` */
export function mockListDirectory(dir: Directory): Promise<ListDirectoryResponseBody> {
  return Promise.resolve({
    assets: ASSET_SPECS.get(dir.id)!.contents.map((id) => MOCK_FS.get(id)!),
    paginationToken: null,
  })
}
