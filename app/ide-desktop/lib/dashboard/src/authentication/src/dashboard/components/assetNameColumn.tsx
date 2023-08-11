/** @file The icon and name of an {@link backendModule.Asset}. */
import * as React from 'react'

import * as backendModule from '../backend'

import * as column from '../column'
import DirectoryNameColumn from './directoryNameColumn'
import FileNameColumn from './fileNameColumn'
import ProjectNameColumn from './projectNameColumn'
import SecretNameColumn from './secretNameColumn'

// =================
// === AssetName ===
// =================

/** Props for a {@link AssetNameColumn}. */
export interface AssetNameColumnProps extends column.AssetColumnProps<backendModule.AnyAsset> {}

/** The icon and name of an {@link backendModule.Asset}. */
export default function AssetNameColumn(props: AssetNameColumnProps) {
    const { item } = props
    switch (item.type) {
        // These type assertions are SAFE, as switching on `item.type` ensures that the `item`
        // is of the correct type.
        /* eslint-disable no-restricted-syntax */
        case backendModule.AssetType.directory: {
            return (
                <DirectoryNameColumn
                    {...(props as column.AssetColumnProps<backendModule.DirectoryAsset>)}
                />
            )
        }
        case backendModule.AssetType.project: {
            return (
                <ProjectNameColumn
                    {...(props as column.AssetColumnProps<backendModule.ProjectAsset>)}
                />
            )
        }
        case backendModule.AssetType.file: {
            return (
                <FileNameColumn {...(props as column.AssetColumnProps<backendModule.FileAsset>)} />
            )
        }
        case backendModule.AssetType.secret: {
            return (
                <SecretNameColumn
                    {...(props as column.AssetColumnProps<backendModule.SecretAsset>)}
                />
            )
        }
        case backendModule.AssetType.specialLoading:
        case backendModule.AssetType.specialEmpty: {
            // Special rows do not display columns at all.
            return <></>
        }
        /* eslint-enable no-restricted-syntax */
    }
}
