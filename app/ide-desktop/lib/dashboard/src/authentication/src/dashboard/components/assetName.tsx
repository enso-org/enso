/** @file The icon and name of an {@link backendModule.Asset}. */
import * as React from 'react'

import * as backendModule from '../backend'

import * as assetsTable from './assetsTable'
import * as tableColumn from './tableColumn'
import DirectoryName from './directoryName'
import FileName from './fileName'
import ProjectName from './projectName'
import SecretName from './secretName'

// =================
// === AssetName ===
// =================

/** Props for a {@link AssetName}. */
export interface AssetNameProps<T extends backendModule.AnyAsset>
    extends tableColumn.TableColumnProps<
        T,
        assetsTable.AssetsTableState,
        assetsTable.AssetRowState
    > {}

/** The icon and name of an {@link backendModule.Asset}. */
export default function AssetName(props: AssetNameProps<backendModule.AnyAsset>) {
    const { item } = props
    switch (item.type) {
        // These type assertions are SAFE, as switching on `item.type` ensures that the `item`
        // is of the correct type.
        /* eslint-disable no-restricted-syntax */
        case backendModule.AssetType.directory: {
            return <DirectoryName {...(props as AssetNameProps<backendModule.DirectoryAsset>)} />
        }
        case backendModule.AssetType.project: {
            return <ProjectName {...(props as AssetNameProps<backendModule.ProjectAsset>)} />
        }
        case backendModule.AssetType.file: {
            return <FileName {...(props as AssetNameProps<backendModule.FileAsset>)} />
        }
        case backendModule.AssetType.secret: {
            return <SecretName {...(props as AssetNameProps<backendModule.SecretAsset>)} />
        }
        /* eslint-enable no-restricted-syntax */
    }
}
