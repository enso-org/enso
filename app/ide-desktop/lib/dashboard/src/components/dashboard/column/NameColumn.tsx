/** @file The icon and name of an {@link backendModule.Asset}. */
import * as React from 'react'

import type * as column from '#/components/dashboard/column'
import DatalinkNameColumn from '#/components/dashboard/DatalinkNameColumn'
import DirectoryNameColumn from '#/components/dashboard/DirectoryNameColumn'
import FileNameColumn from '#/components/dashboard/FileNameColumn'
import ProjectNameColumn from '#/components/dashboard/ProjectNameColumn'
import SecretNameColumn from '#/components/dashboard/SecretNameColumn'

import * as backendModule from '#/services/Backend'

// ==================
// === NameColumn ===
// ==================

/** Props for a {@link NameColumn}. */
export interface NameColumnProps extends column.AssetColumnProps {}

/** The icon and name of an {@link backendModule.AnyAsset}. */
export default function NameColumn(props: NameColumnProps) {
  const { item } = props
  switch (item.type) {
    case backendModule.AssetType.directory: {
      return <DirectoryNameColumn {...props} />
    }
    case backendModule.AssetType.project: {
      return <ProjectNameColumn {...props} />
    }
    case backendModule.AssetType.file: {
      return <FileNameColumn {...props} />
    }
    case backendModule.AssetType.datalink: {
      return <DatalinkNameColumn {...props} />
    }
    case backendModule.AssetType.secret: {
      return <SecretNameColumn {...props} />
    }
  }
}
