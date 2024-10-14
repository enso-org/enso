/** @file The icon and name of an {@link backendModule.Asset}. */
import * as React from 'react'

import type * as column from '#/components/dashboard/column'
import DatalinkNameColumn from '#/components/dashboard/DatalinkNameColumn'
import DirectoryNameColumn from '#/components/dashboard/DirectoryNameColumn'
import FileNameColumn from '#/components/dashboard/FileNameColumn'
import ProjectNameColumn from '#/components/dashboard/ProjectNameColumn'
import SecretNameColumn from '#/components/dashboard/SecretNameColumn'

import * as backendModule from '#/services/Backend'

// =================
// === AssetName ===
// =================

/** Props for a {@link AssetNameColumn}. */
export type AssetNameColumnProps = column.AssetColumnProps

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
    case backendModule.AssetType.specialLoading:
    case backendModule.AssetType.specialEmpty:
    case backendModule.AssetType.specialError: {
      // Special rows do not display columns at all.
      return <></>
    }
  }
}
