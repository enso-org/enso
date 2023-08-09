/** @file The icon and name of a {@link backendModule.ProjectAsset}. */
import * as React from 'react'

import * as assetEventModule from '../events/assetEvent'
import * as assetListEventModule from '../events/assetListEvent'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as eventModule from '../event'
import * as hooks from '../../hooks'
import * as indent from '../indent'
import * as presence from '../presence'
import * as shortcuts from '../shortcuts'
import * as validation from '../validation'

import * as column from '../column'
import EditableSpan from './editableSpan'
import ProjectIcon from './projectIcon'

// ===================
// === ProjectName ===
// ===================

/** Props for a {@link ProjectNameColumn}. */
export interface ProjectNameColumnProps
    extends column.AssetColumnProps<backendModule.ProjectAsset> {}

/** The icon and name of a {@link backendModule.ProjectAsset}. */
export default function ProjectNameColumn(props: ProjectNameColumnProps) {
    const {
        keyProp: key,
        item,
        setItem,
        selected,
        rowState,
        setRowState,
        state: {
            appRunner,
            assetEvents,
            dispatchAssetEvent,
            dispatchAssetListEvent,
            doOpenManually,
            doOpenIde,
            doCloseIde,
            getDepth,
        },
    } = props
    const { backend } = backendProvider.useBackend()
    const toastAndLog = hooks.useToastAndLog()

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
            toastAndLog('Unable to rename project', error)
            throw error
        }
    }

    hooks.useEventHandler(assetEvents, async event => {
        switch (event.type) {
            case assetEventModule.AssetEventType.createDirectory:
            case assetEventModule.AssetEventType.createSecret:
            case assetEventModule.AssetEventType.openProject:
            case assetEventModule.AssetEventType.cancelOpeningAllProjects:
            case assetEventModule.AssetEventType.deleteMultiple:
            case assetEventModule.AssetEventType.downloadSelected: {
                // Ignored. Any missing project-related events should be handled by `ProjectIcon`.
                // `deleteMultiple` and `downloadSelected` are handled by `AssetRow`.
                break
            }
            case assetEventModule.AssetEventType.createProject: {
                // This should only run before this project gets replaced with the actual project
                // by this event handler. In both cases `key` will match, so using `key` here
                // is a mistake.
                if (item.id === event.placeholderId) {
                    rowState.setPresence(presence.Presence.inserting)
                    try {
                        const createdProject = await backend.createProject({
                            parentDirectoryId: item.parentId,
                            projectName: item.title,
                            projectTemplateName: event.templateId,
                        })
                        rowState.setPresence(presence.Presence.present)
                        setItem({
                            ...item,
                            id: createdProject.projectId,
                            projectState: { type: backendModule.ProjectState.placeholder },
                        })
                        dispatchAssetEvent({
                            type: assetEventModule.AssetEventType.openProject,
                            id: createdProject.projectId,
                        })
                    } catch (error) {
                        dispatchAssetListEvent({
                            type: assetListEventModule.AssetListEventType.delete,
                            id: key,
                        })
                        toastAndLog('Error creating new project', error)
                    }
                }
                break
            }
            case assetEventModule.AssetEventType.uploadFiles: {
                const file = event.files.get(key)
                if (file != null) {
                    rowState.setPresence(presence.Presence.inserting)
                    try {
                        if (backend.type === backendModule.BackendType.local) {
                            /** Information required to display a bundle. */
                            interface BundleInfo {
                                name: string
                                id: string
                            }
                            // This non-standard property is defined in Electron.
                            let info: BundleInfo
                            if (
                                'backendApi' in window &&
                                'path' in file &&
                                typeof file.path === 'string'
                            ) {
                                info = await window.backendApi.importProjectFromPath(file.path)
                            } else {
                                const response = await fetch('./api/upload-project', {
                                    method: 'POST',
                                    // Ideally this would use `file.stream()`, to minimize RAM
                                    // requirements. for uploading large projects. Unfortunately,
                                    // this requires HTTP/2, which is HTTPS-only, so it will not
                                    // work on `http://localhost`.
                                    body: await file.arrayBuffer(),
                                })
                                // This is SAFE, as the types of this API are statically known.
                                // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
                                info = await response.json()
                            }
                            rowState.setPresence(presence.Presence.present)
                            setItem({
                                ...item,
                                title: info.name,
                                id: backendModule.ProjectId(info.id),
                            })
                        } else {
                            const fileName = item.title
                            const title = backendModule.stripProjectExtension(item.title)
                            setItem({
                                ...item,
                                title,
                            })
                            const createdFile = await backend.uploadFile(
                                {
                                    fileId: null,
                                    fileName,
                                    parentDirectoryId: item.parentId,
                                },
                                file
                            )
                            const project = createdFile.project
                            if (project == null) {
                                throw new Error('The uploaded file was not a project.')
                            } else {
                                rowState.setPresence(presence.Presence.present)
                                setItem({
                                    ...item,
                                    title,
                                    id: project.projectId,
                                    projectState: project.state,
                                })
                                return
                            }
                        }
                    } catch (error) {
                        dispatchAssetListEvent({
                            type: assetListEventModule.AssetListEventType.delete,
                            id: key,
                        })
                        toastAndLog('Could not upload project', error)
                    }
                }
                break
            }
        }
    })

    return (
        <div
            className={`flex text-left items-center whitespace-nowrap ${indent.indentClass(
                getDepth(key)
            )}`}
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
                keyProp={key}
                item={item}
                setItem={setItem}
                assetEvents={assetEvents}
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
