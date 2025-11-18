import { type Directory } from '@/components/widgets/FileBrowserWidget/pathBrowsing'
import {
  type AnyAsset,
  AssetType,
  type ListDirectoryResponseBody,
} from 'enso-common/src/services/Backend'

interface MockAssetSpec {
  type: AssetType
  title: string
  contents: string[]
}
const ASSET_SPECS = new Map([
  [
    '0',
    {
      type: AssetType.directory,
      title: '',
      contents: ['1', '2'],
    },
  ],
  [
    '1',
    {
      type: AssetType.directory,
      title: 'Users',
      contents: ['3'],
    },
  ],
  [
    '2',
    {
      type: AssetType.directory,
      title: 'Teams',
      contents: [],
    },
  ],
  [
    '3',
    {
      type: AssetType.directory,
      title: 'user',
      contents: ['4', '5'],
    },
  ],
  [
    '4',
    {
      type: AssetType.directory,
      title: 'New Folder 1',
      contents: ['6', '7'],
    },
  ],
  [
    '5',
    {
      type: AssetType.file,
      title: 'input.csv',
      contents: [],
    },
  ],
  [
    '6',
    {
      type: AssetType.file,
      title: 'input.csv',
      contents: [],
    },
  ],
  [
    '7',
    {
      type: AssetType.directory,
      title: 'Nested',
      contents: ['8'],
    },
  ],
  [
    '8',
    {
      type: AssetType.file,
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
