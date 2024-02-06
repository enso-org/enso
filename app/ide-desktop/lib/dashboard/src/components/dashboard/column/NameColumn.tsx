/** @file The icon and name of an {@link backendModule.Asset}. */
import * as React from 'react'

import type * as column from '#/components/dashboard/column'
import DirectoryNameColumn from '#/components/dashboard/DirectoryNameColumn'
import FileNameColumn from '#/components/dashboard/FileNameColumn'
import ProjectNameColumn from '#/components/dashboard/ProjectNameColumn'
import SecretNameColumn from '#/components/dashboard/SecretNameColumn'

import * as backendModule from '#/services/Backend'

// =================
// === AssetName ===
// =================

/** Props for a {@link AssetNameColumn}. */
export interface AssetNameColumnProps extends column.AssetColumnProps {}

/** The icon and name of an {@link backendModule.Asset}. */
export default function AssetNameColumn(props: AssetNameColumnProps) {
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
