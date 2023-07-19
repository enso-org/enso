/** @file The icon and name of a {@link backendModule.ProjectAsset}. */
import * as React from 'react'
import toast from 'react-hot-toast'

import * as assetEventModule from '../events/assetEvent'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as errorModule from '../../error'
import * as eventModule from '../event'
import * as loggerProvider from '../../providers/logger'
import * as shortcuts from '../shortcuts'
import * as validation from '../validation'

import * as assetsTable from './assetsTable'
import * as tableColumn from './tableColumn'
import EditableSpan from './editableSpan'
import ProjectIcon from './projectIcon'

// ===================
// === ProjectName ===
// ===================

/** Props for a {@link ProjectName}. */
interface InternalProjectNameProps
    extends tableColumn.TableColumnProps<
        backendModule.ProjectAsset,
        assetsTable.AssetsTableState,
        assetsTable.AssetRowState
    > {}

/** The icon and name of a {@link backendModule.ProjectAsset}. */
export default function ProjectName(props: InternalProjectNameProps) {
    const {
        item,
        setItem,
        selected,
        rowState,
        setRowState,
        state: { appRunner, assetEvent, dispatchAssetEvent, doOpenManually, doOpenIde, doCloseIde },
    } = props
    const logger = loggerProvider.useLogger()
    const { backend } = backendProvider.useBackend()

    const doRename = async (newName: string) => {
        try {
            await backend.projectUpdate(
                item.id,
                {
                    ami: null,
                    ideVersion: null,
                    projectName: newName,
                },
                item.title
            )
            return
        } catch (error) {
            const message = `Unable to rename project: ${
                errorModule.tryGetMessage(error) ?? 'unknown error.'
            }`
            toast.error(message)
            logger.error(message)
            throw error
        }
    }

    return (
        <div
            className="flex text-left items-center align-middle whitespace-nowrap"
            onClick={event => {
                if (!rowState.isEditingName && eventModule.isDoubleClick(event)) {
                    // It is a double click; open the project.
                    dispatchAssetEvent({
                        type: assetEventModule.AssetEventType.openProject,
                        id: item.id,
                    })
                } else if (
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
            <ProjectIcon
                project={item}
                rowState={rowState}
                setRowState={setRowState}
                assetEvent={assetEvent}
                doOpenManually={doOpenManually}
                appRunner={appRunner}
                openIde={() => {
                    doOpenIde(item)
                }}
                onClose={doCloseIde}
            />
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
                {...(backend.type === backendModule.BackendType.local
                    ? {
                          inputPattern: validation.LOCAL_PROJECT_NAME_PATTERN,
                          inputTitle: validation.LOCAL_PROJECT_NAME_TITLE,
                      }
                    : {})}
                className={`bg-transparent grow px-2 ${
                    rowState.isEditingName ? 'cursor-text' : 'cursor-pointer'
                }`}
            >
                {item.title}
            </EditableSpan>
        </div>
    )
}
