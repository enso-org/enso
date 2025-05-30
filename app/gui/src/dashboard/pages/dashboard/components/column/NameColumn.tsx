/** @file The icon and name of an {@link backendModule.Asset}. */
import type { AssetColumnProps } from '#/pages/dashboard/components/column'
import DatalinkNameColumn from '#/pages/dashboard/components/DatalinkNameColumn'
import DirectoryNameColumn from '#/pages/dashboard/components/DirectoryNameColumn'
import FileNameColumn from '#/pages/dashboard/components/FileNameColumn'
import ProjectNameColumn from '#/pages/dashboard/components/ProjectNameColumn'
import SecretNameColumn from '#/pages/dashboard/components/SecretNameColumn'

import * as backendModule from '#/services/Backend'

/** Props for a {@link AssetNameColumn}. */
export type AssetNameColumnProps = AssetColumnProps

/** The icon and name of an {@link backendModule.Asset}. */
export default function AssetNameColumn(props: AssetNameColumnProps) {
  const { item } = props

  switch (item.type) {
    case backendModule.AssetType.directory: {
      return <DirectoryNameColumn {...props} item={item} />
    }
    case backendModule.AssetType.project: {
      return <ProjectNameColumn {...props} item={item} />
    }
    case backendModule.AssetType.file: {
      return <FileNameColumn {...props} item={item} />
    }
    case backendModule.AssetType.datalink: {
      return <DatalinkNameColumn {...props} item={item} />
    }
    case backendModule.AssetType.secret: {
      return <SecretNameColumn {...props} item={item} />
    }
    case backendModule.AssetType.specialUp:
    case backendModule.AssetType.specialLoading:
    case backendModule.AssetType.specialEmpty:
    case backendModule.AssetType.specialError: {
      // Special rows do not display columns at all.
      return <></>
    }
  }
}
