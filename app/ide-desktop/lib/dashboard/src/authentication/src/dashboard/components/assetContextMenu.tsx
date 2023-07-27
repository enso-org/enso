/** @file The context menu for an arbitrary {@link backendModule.Asset}. */
import * as React from 'react'

import * as assetEventModule from '../events/assetEvent'
import * as backendModule from '../backend'

import * as assetsTable from './assetsTable'
import * as tableRow from './tableRow'
import DirectoryContextMenu from './directoryContextMenu'
import FileContextMenu from './fileContextMenu'
import ProjectContextMenu from './projectContextMenu'
import SecretContextMenu from './secretContextMenu'

/** Props for a {@link AssetContextMenu}. */
export interface AssetContextMenuProps<T extends backendModule.AnyAsset> {
    innerProps: tableRow.TableRowInnerProps<T, assetsTable.AssetRowState, T['id']>
    event: React.MouseEvent
    dispatchAssetEvent: (assetEvent: assetEventModule.AssetEvent) => void
    doDelete: () => Promise<void>
}

/** The context menu for an arbitrary {@link backendModule.Asset}. */
export default function AssetContextMenu(props: AssetContextMenuProps<backendModule.AnyAsset>) {
    switch (props.innerProps.item.type) {
        // The type assertions are SAFE, as the `item.type` matches.
        /* eslint-disable no-restricted-syntax */
        case backendModule.AssetType.directory: {
            return (
                <DirectoryContextMenu
                    {...(props as AssetContextMenuProps<backendModule.DirectoryAsset>)}
                />
            )
        }
        case backendModule.AssetType.project: {
            return (
                <ProjectContextMenu
                    {...(props as AssetContextMenuProps<backendModule.ProjectAsset>)}
                />
            )
        }
        case backendModule.AssetType.file: {
            return (
                <FileContextMenu {...(props as AssetContextMenuProps<backendModule.FileAsset>)} />
            )
        }
        case backendModule.AssetType.secret: {
            return (
                <SecretContextMenu
                    {...(props as AssetContextMenuProps<backendModule.SecretAsset>)}
                />
            )
        }
        case backendModule.AssetType.specialLoading:
        case backendModule.AssetType.specialEmpty: {
            return null
        }
        /* eslint-enable no-restricted-syntax */
    }
}
