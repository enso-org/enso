/** @file The icon and name of a {@link backendModule.SecretAsset}. */
import * as React from 'react'

import SecretIcon from 'enso-assets/secret.svg'

import * as backendModule from '../backend'
import * as eventModule from '../event'
import * as shortcuts from '../shortcuts'

import * as assetName from './assetName'
import EditableSpan from './editableSpan'

// ==================
// === SecretName ===
// ==================

/** Props for a {@link SecretName}. */
export interface SecretNameProps extends assetName.AssetNameProps<backendModule.SecretAsset> {}

/** The icon and name of a {@link backendModule.SecretAsset}. */
export default function SecretName(props: SecretNameProps) {
    const { item, setItem, selected, setRowState } = props

    // TODO[sb]: Wait for backend implementation. `editable` should also be re-enabled, and the
    // context menu entry should be re-added.
    // Backend implementation is tracked here: https://github.com/enso-org/cloud-v2/issues/505.
    const doRename = async (/* _newName: string */) => {
        await Promise.resolve(null)
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
            <img src={SecretIcon} />{' '}
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
