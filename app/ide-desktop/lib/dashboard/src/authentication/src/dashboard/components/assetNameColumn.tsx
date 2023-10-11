/** @file The icon and name of an {@link backendModule.Asset}. */
import * as React from 'react'

import * as backendModule from '../backend'

import type * as column from '../column'
import ConnectorNameColumn from './connectorNameColumn'
import DirectoryNameColumn from './directoryNameColumn'
import FileNameColumn from './fileNameColumn'
import ProjectNameColumn from './projectNameColumn'

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
            return <ConnectorNameColumn {...props} />
        }
        case backendModule.AssetType.specialLoading:
        case backendModule.AssetType.specialEmpty: {
            // Special rows do not display columns at all.
            return <></>
        }
    }
}
