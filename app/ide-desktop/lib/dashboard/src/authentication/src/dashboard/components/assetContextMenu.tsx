/** @file The context menu for an arbitrary {@link backendModule.Asset}. */
import * as React from 'react'

import * as assetEventModule from '../events/assetEvent'
import * as backendModule from '../backend'

import * as assetsTable from './assetsTable'
import * as tableRow from './tableRow'
import ContextMenus from './contextMenus'
import DirectoryContextMenu from './directoryContextMenu'
import FileContextMenu from './fileContextMenu'
import GlobalContextMenu from './globalContextMenu'
import ProjectContextMenu from './projectContextMenu'
import SecretContextMenu from './secretContextMenu'

/** Props for a {@link AssetContextMenu}. */
export interface AssetContextMenuProps<T extends backendModule.AnyAsset> {
    innerProps: tableRow.TableRowInnerProps<
        T,
        assetsTable.AssetsTableState,
        assetsTable.AssetRowState,
        T['id']
    >
    event: React.MouseEvent
    dispatchAssetEvent: (assetEvent: assetEventModule.AssetEvent) => void
    doDelete: () => Promise<void>
}

/** The context menu for an arbitrary {@link backendModule.Asset}. */
export default function AssetContextMenu(props: AssetContextMenuProps<backendModule.AnyAsset>) {
    let contextMenu: JSX.Element | null
    switch (props.innerProps.item.type) {
        // The type assertions are SAFE, as the `item.type` matches.
        /* eslint-disable no-restricted-syntax */
        case backendModule.AssetType.directory: {
            contextMenu = (
                <DirectoryContextMenu
                    {...(props as AssetContextMenuProps<backendModule.DirectoryAsset>)}
                />
            )
            break
        }
        case backendModule.AssetType.project: {
            contextMenu = (
                <ProjectContextMenu
                    {...(props as AssetContextMenuProps<backendModule.ProjectAsset>)}
                />
            )
            break
        }
        case backendModule.AssetType.file: {
            contextMenu = (
                <FileContextMenu {...(props as AssetContextMenuProps<backendModule.FileAsset>)} />
            )
            break
        }
        case backendModule.AssetType.secret: {
            contextMenu = (
                <SecretContextMenu
                    {...(props as AssetContextMenuProps<backendModule.SecretAsset>)}
                />
            )
            break
        }
        case backendModule.AssetType.specialLoading:
        case backendModule.AssetType.specialEmpty: {
            contextMenu = null
            break
        }
        /* eslint-enable no-restricted-syntax */
    }
    return (
        <ContextMenus key={props.innerProps.item.id} event={props.event}>
            {contextMenu}
            <GlobalContextMenu {...props} />
        </ContextMenus>
    )
}
