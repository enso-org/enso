/** @file The icon and name of an {@link Asset}. */
import DatalinkNameColumn from '#/components/dashboard/DatalinkNameColumn'
import DirectoryNameColumn from '#/components/dashboard/DirectoryNameColumn'
import FileNameColumn from '#/components/dashboard/FileNameColumn'
import ProjectNameColumn from '#/components/dashboard/ProjectNameColumn'
import SecretNameColumn from '#/components/dashboard/SecretNameColumn'
import { AssetType, type Asset } from '#/services/Backend'
import type { AssetColumnProps } from './types'

/** Props for a {@link AssetNameColumn}. */
export type AssetNameColumnProps = AssetColumnProps

/** The icon and name of an {@link Asset}. */
export default function AssetNameColumn(props: AssetNameColumnProps) {
  const { item } = props

  switch (item.type) {
    case AssetType.directory: {
      return <DirectoryNameColumn {...props} item={item} />
    }
    case AssetType.project: {
      return <ProjectNameColumn {...props} item={item} />
    }
    case AssetType.file: {
      return <FileNameColumn {...props} item={item} />
    }
    case AssetType.datalink: {
      return <DatalinkNameColumn {...props} item={item} />
    }
    case AssetType.secret: {
      return <SecretNameColumn {...props} item={item} />
    }
    case AssetType.specialLoading:
    case AssetType.specialEmpty:
    case AssetType.specialError: {
      // Special rows do not display columns at all.
      return <></>
    }
  }
}
