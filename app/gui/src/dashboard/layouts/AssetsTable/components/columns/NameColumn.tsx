/** @file The icon and name of an {@link Asset}. */
import { AssetType } from '#/services/Backend'
import type { AssetColumnProps } from './columnProps'
import DatalinkNameColumn from './name/DatalinkNameColumn'
import DirectoryNameColumn from './name/DirectoryNameColumn'
import FileNameColumn from './name/FileNameColumn'
import ProjectNameColumn from './name/ProjectNameColumn'
import SecretNameColumn from './name/SecretNameColumn'

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
    case AssetType.specialUp:
    case AssetType.specialLoading:
    case AssetType.specialEmpty:
    case AssetType.specialError: {
      // Special rows do not display columns at all.
      return <></>
    }
  }
}
