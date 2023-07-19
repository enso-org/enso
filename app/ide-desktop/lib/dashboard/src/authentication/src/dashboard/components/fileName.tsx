/** @file The icon and name of a {@link backendModule.FileAsset}. */
import * as React from 'react'

import * as backendModule from '../backend'
import * as eventModule from '../event'
import * as fileInfo from '../../fileInfo'
import * as shortcuts from '../shortcuts'

import * as assetName from './assetName'
import EditableSpan from './editableSpan'

// ================
// === FileName ===
// ================

/** Props for a {@link FileName}. */
export interface FileNameProps extends assetName.AssetNameProps<backendModule.FileAsset> {}

/** The icon and name of a {@link backendModule.FileAsset}. */
export default function FileName(props: FileNameProps) {
    const { item, setItem, selected, setRowState } = props

    // TODO[sb]: Wait for backend implementation. `editable` should also be re-enabled, and the
    // context menu entry should be re-added.
    // Backend implementation is tracked here: https://github.com/enso-org/cloud-v2/issues/505.
    const doRename = async () => {
        return await Promise.resolve(null)
    }

    return (
        <div
            className="flex text-left items-center align-middle whitespace-nowrap"
            onClick={event => {
                if (
                    eventModule.isSingleClick(event) &&
                    (selected ||
                        shortcuts.SHORTCUT_REGISTRY.matchesMouseAction(
                            shortcuts.MouseAction.editName,
                            event
                        ))
                ) {
                    setRowState(oldRowState => ({
                        ...oldRowState,
                        isEditingName: true,
                    }))
                }
            }}
        >
            <img src={fileInfo.fileIcon()} />
            <EditableSpan
                editable={false}
                onSubmit={async newTitle => {
                    setRowState(oldRowState => ({
                        ...oldRowState,
                        isEditingName: false,
                    }))
                    if (newTitle !== item.title) {
                        const oldTitle = item.title
                        setItem(oldItem => ({ ...oldItem, title: newTitle }))
                        try {
                            await doRename(/* newTitle */)
                        } catch {
                            setItem(oldItem => ({ ...oldItem, title: oldTitle }))
                        }
                    }
                }}
                onCancel={() => {
                    setRowState(oldRowState => ({
                        ...oldRowState,
                        isEditingName: false,
                    }))
                }}
                className="bg-transparent grow px-2"
            >
                {item.title}
            </EditableSpan>
        </div>
    )
}
