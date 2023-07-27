/** @file A context menu available everywhere in the directory. */
import * as React from 'react'

import AddConnectorIcon from 'enso-assets/add_connector.svg'
import AddFolderIcon from 'enso-assets/add_folder.svg'
import AddNetworkIcon from 'enso-assets/add_network.svg'
import DataUploadIcon from 'enso-assets/data_upload.svg'

import * as assetListEventModule from '../events/assetListEvent'
import * as backend from '../backend'

import * as assetContextMenu from './assetContextMenu'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'

/** Props for a {@link GlobalContextMenu}. */
export interface GlobalContextMenuProps
    extends assetContextMenu.AssetContextMenuProps<backend.AnyAsset> {}

/** A context menu available everywhere in the directory. */
export default function GlobalContextMenu(props: GlobalContextMenuProps) {
    const {
        innerProps: {
            item,
            state: { dispatchAssetListEvent },
        },
    } = props
    return (
        <ContextMenu>
            <ContextMenuEntry
                icon={DataUploadIcon}
                onClick={() => {
                    // Ignored; the event is handled by the HTML `input` element.
                }}
            >
                <input
                    multiple
                    type="file"
                    id="context_menu_file_input"
                    className="hidden"
                    onInput={event => {
                        if (event.currentTarget.files != null) {
                            dispatchAssetListEvent({
                                type: assetListEventModule.AssetListEventType.uploadFiles,
                                parentId: item.parentId,
                                files: event.currentTarget.files,
                            })
                        }
                    }}
                ></input>
                <label htmlFor="context_menu_file_input">Upload Files</label>
            </ContextMenuEntry>
            <ContextMenuEntry
                icon={AddNetworkIcon}
                onClick={() => {
                    dispatchAssetListEvent({
                        type: assetListEventModule.AssetListEventType.createProject,
                        parentId: item.parentId,
                        templateId: null,
                    })
                }}
            >
                New Project
            </ContextMenuEntry>
            <ContextMenuEntry
                icon={AddFolderIcon}
                onClick={() => {
                    dispatchAssetListEvent({
                        type: assetListEventModule.AssetListEventType.createDirectory,
                        parentId: item.parentId,
                    })
                }}
            >
                New Folder
            </ContextMenuEntry>
            <ContextMenuEntry
                disabled
                icon={AddConnectorIcon}
                onClick={() => {
                    // No backend support yet.
                }}
            >
                New Data Connector
            </ContextMenuEntry>
        </ContextMenu>
    )
}
