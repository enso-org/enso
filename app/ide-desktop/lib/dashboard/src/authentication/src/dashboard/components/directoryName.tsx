/** @file The icon and name of a {@link backendModule.DirectoryAsset}. */
import * as React from 'react'
import toast from 'react-hot-toast'

import DirectoryIcon from 'enso-assets/directory.svg'

import * as backendModule from '../backend'
import * as errorModule from '../../error'
import * as eventModule from '../event'
import * as shortcuts from '../shortcuts'

import * as backendProvider from '../../providers/backend'
import * as loggerProvider from '../../providers/logger'

import * as assetName from './assetName'
import EditableSpan from './editableSpan'

// =====================
// === DirectoryName ===
// =====================

/** Props for a {@link DirectoryName}. */
export interface DirectoryNameProps
    extends assetName.AssetNameProps<backendModule.DirectoryAsset> {}

/** The icon and name of a {@link backendModule.DirectoryAsset}. */
export default function DirectoryName(props: DirectoryNameProps) {
    const {
        item,
        setItem,
        selected,
        state: { toggleDirectoryExpansion },
        rowState,
        setRowState,
    } = props
    const logger = loggerProvider.useLogger()
    const { backend } = backendProvider.useBackend()

    const doRename = async (newName: string) => {
        if (backend.type !== backendModule.BackendType.local) {
            try {
                await backend.updateDirectory(item.id, { title: newName }, item.title)
                return
            } catch (error) {
                const message = `Error renaming folder: ${
                    errorModule.tryGetMessage(error) ?? 'unknown error'
                }`
                toast.error(message)
                logger.error(message)
                throw error
            }
        }
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
                } else if (eventModule.isDoubleClick(event)) {
                    toggleDirectoryExpansion(item)
                }
            }}
        >
            <img src={DirectoryIcon} />
            <EditableSpan
                editable={rowState.isEditingName}
                onSubmit={async newTitle => {
                    setRowState(oldRowState => ({
                        ...oldRowState,
                        isEditingName: false,
                    }))
                    if (newTitle !== item.title) {
                        const oldTitle = item.title
                        setItem(oldItem => ({ ...oldItem, title: newTitle }))
                        try {
                            await doRename(newTitle)
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
                className="cursor-pointer bg-transparent grow px-2"
            >
                {item.title}
            </EditableSpan>
        </div>
    )
}
