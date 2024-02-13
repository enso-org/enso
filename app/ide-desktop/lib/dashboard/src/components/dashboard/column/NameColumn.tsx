/** @file The icon and name of an {@link backendModule.AnyAsset}. */
import * as React from 'react'

import type * as column from '#/components/dashboard/column'
import DataLinkNameColumn from '#/components/dashboard/column/DataLinkNameColumn'
import DirectoryNameColumn from '#/components/dashboard/column/DirectoryNameColumn'
import FileNameColumn from '#/components/dashboard/column/FileNameColumn'
import ProjectNameColumn from '#/components/dashboard/column/ProjectNameColumn'
import SecretNameColumn from '#/components/dashboard/column/SecretNameColumn'

import * as backendModule from '#/services/Backend'

// ==================
// === NameColumn ===
// ==================

/** Props for a {@link NameColumn}. */
export interface NameColumnProps extends column.AssetColumnProps {}

/** The icon and name of an {@link backendModule.AnyAsset}. */
export default function NameColumn(props: NameColumnProps) {
  const { item } = props
  switch (item.item.type) {
    case backendModule.AssetType.directory: {
      return <DirectoryNameColumn {...props} />
    }
    case backendModule.AssetType.project: {
      return <ProjectNameColumn {...props} />
    }
    case backendModule.AssetType.file: {
      return <FileNameColumn {...props} />
    }
    case backendModule.AssetType.dataLink: {
      return <DataLinkNameColumn {...props} />
    }
    case backendModule.AssetType.secret: {
      return <SecretNameColumn {...props} />
    }
    case backendModule.AssetType.specialLoading:
    case backendModule.AssetType.specialEmpty: {
      // Special rows do not display columns at all.
      return <></>
    }
  }
}
