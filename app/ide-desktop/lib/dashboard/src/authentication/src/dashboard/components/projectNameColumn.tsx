/** @file The icon and name of a {@link backendModule.ProjectAsset}. */
import * as React from 'react'

import NetworkIcon from 'enso-assets/network.svg'

import * as assetEventModule from '../events/assetEvent'
import * as assetListEventModule from '../events/assetListEvent'
import * as assetTreeNode from '../assetTreeNode'
import * as authProvider from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as errorModule from '../../error'
import * as eventModule from '../event'
import * as hooks from '../../hooks'
import * as indent from '../indent'
import * as presence from '../presence'
import * as shortcutsModule from '../shortcuts'
import * as shortcutsProvider from '../../providers/shortcuts'
import * as validation from '../validation'

import * as column from '../column'
import EditableSpan from './editableSpan'
import ProjectIcon from './projectIcon'
import SvgMask from '../../authentication/components/svgMask'

// ===================
// === ProjectName ===
// ===================

/** Props for a {@link ProjectNameColumn}. */
export interface ProjectNameColumnProps extends column.AssetColumnProps {}

/** The icon and name of a {@link backendModule.ProjectAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.ProjectAsset}.
 * This should never happen. */
export default function ProjectNameColumn(props: ProjectNameColumnProps) {
    const {
        item,
        setItem,
        selected,
        rowState,
        setRowState,
        state: {
            assetEvents,
            dispatchAssetEvent,
            dispatchAssetListEvent,
            doOpenManually,
            doOpenIde,
            doCloseIde,
        },
    } = props
    const toastAndLog = hooks.useToastAndLog()
    const { backend } = backendProvider.useBackend()
    const { organization } = authProvider.useNonPartialUserSession()
    const { shortcuts } = shortcutsProvider.useShortcuts()
    const asset = item.item
    if (asset.type !== backendModule.AssetType.project) {
        // eslint-disable-next-line no-restricted-syntax
        throw new Error('`ProjectNameColumn` can only display project assets.')
    }
    const setAsset = assetTreeNode.useSetAsset(asset, setItem)
    const ownPermission =
        asset.permissions?.find(permission => permission.user.user_email === organization?.email) ??
        null
    const isRunning = backendModule.DOES_PROJECT_STATE_INDICATE_VM_EXISTS[asset.projectState.type]
    const canExecute =
        backend.type === backendModule.BackendType.local ||
        (ownPermission != null &&
            backendModule.PERMISSION_ACTION_CAN_EXECUTE[ownPermission.permission])
    const isOtherUserUsingProject =
        backend.type !== backendModule.BackendType.local &&
        asset.projectState.opened_by != null &&
        asset.projectState.opened_by !== organization?.email

    const doRename = async (newName: string) => {
        try {
            await backend.projectUpdate(
                asset.id,
                {
                    ami: null,
                    ideVersion: null,
                    projectName: newName,
                },
                asset.title
            )
            return
        } catch (error) {
            toastAndLog(errorModule.tryGetMessage(error) ?? 'Could not rename project.')
            throw error
        }
    }

    hooks.useEventHandler(assetEvents, async event => {
        switch (event.type) {
            case assetEventModule.AssetEventType.newFolder:
            case assetEventModule.AssetEventType.newSecret:
            case assetEventModule.AssetEventType.openProject:
            case assetEventModule.AssetEventType.closeProject:
            case assetEventModule.AssetEventType.cancelOpeningAllProjects:
            case assetEventModule.AssetEventType.deleteMultiple:
            case assetEventModule.AssetEventType.downloadSelected:
            case assetEventModule.AssetEventType.removeSelf: {
                // Ignored. Any missing project-related events should be handled by `ProjectIcon`.
                // `deleteMultiple` and `downloadSelected` are handled by `AssetRow`.
                break
            }
            case assetEventModule.AssetEventType.newProject: {
                // This should only run before this project gets replaced with the actual project
                // by this event handler. In both cases `key` will match, so using `key` here
                // is a mistake.
                if (asset.id === event.placeholderId) {
                    rowState.setPresence(presence.Presence.inserting)
                    try {
                        const createdProject = await backend.createProject({
                            parentDirectoryId: asset.parentId,
                            projectName: asset.title,
                            projectTemplateName: event.templateId,
                        })
                        rowState.setPresence(presence.Presence.present)
                        setAsset({
                            ...asset,
                            id: createdProject.projectId,
                            projectState: {
                                ...asset.projectState,
                                type: backendModule.ProjectState.placeholder,
                            },
                        })
                        dispatchAssetEvent({
                            type: assetEventModule.AssetEventType.openProject,
                            id: createdProject.projectId,
                            shouldAutomaticallySwitchPage: true,
                            runInBackground: false,
                        })
                    } catch (error) {
                        dispatchAssetListEvent({
                            type: assetListEventModule.AssetListEventType.delete,
                            key: item.key,
                        })
                        toastAndLog('Error creating new project', error)
                    }
                }
                break
            }
            case assetEventModule.AssetEventType.uploadFiles: {
                const file = event.files.get(item.key)
                if (file != null) {
                    rowState.setPresence(presence.Presence.inserting)
                    try {
                        if (backend.type === backendModule.BackendType.local) {
                            let id: string
                            if (
                                'backendApi' in window &&
                                // This non-standard property is defined in Electron.
                                'path' in file &&
                                typeof file.path === 'string'
                            ) {
                                id = await window.backendApi.importProjectFromPath(file.path)
                            } else {
                                const response = await fetch('./api/upload-project', {
                                    method: 'POST',
                                    // Ideally this would use `file.stream()`, to minimize RAM
                                    // requirements. for uploading large projects. Unfortunately,
                                    // this requires HTTP/2, which is HTTPS-only, so it will not
                                    // work on `http://localhost`.
                                    body: await file.arrayBuffer(),
                                })
                                id = await response.text()
                            }
                            const listedProject = await backend.getProjectDetails(
                                backendModule.ProjectId(id),
                                null
                            )
                            rowState.setPresence(presence.Presence.present)
                            setAsset({
                                ...asset,
                                title: listedProject.packageName,
                                id: backendModule.ProjectId(id),
                            })
                        } else {
                            const fileName = asset.title
                            const title = backendModule.stripProjectExtension(asset.title)
                            setAsset({
                                ...asset,
                                title,
                            })
                            const createdFile = await backend.uploadFile(
                                {
                                    fileId: null,
                                    fileName,
                                    parentDirectoryId: asset.parentId,
                                },
                                file
                            )
                            const project = createdFile.project
                            if (project == null) {
                                throw new Error('The uploaded file was not a project.')
                            } else {
                                rowState.setPresence(presence.Presence.present)
                                setAsset({
                                    ...asset,
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
                            key: item.key,
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
            className={`flex text-left items-center whitespace-nowrap rounded-l-full gap-1 px-1.5 py-1 min-w-max ${indent.indentClass(
                item.depth
            )}`}
            onClick={event => {
                if (rowState.isEditingName || isOtherUserUsingProject) {
                    // The project should neither be edited nor opened in these cases.
                } else if (shortcuts.matchesMouseAction(shortcutsModule.MouseAction.open, event)) {
                    // It is a double click; open the project.
                    dispatchAssetEvent({
                        type: assetEventModule.AssetEventType.openProject,
                        id: asset.id,
                        shouldAutomaticallySwitchPage: true,
                        runInBackground: false,
                    })
                } else if (shortcuts.matchesMouseAction(shortcutsModule.MouseAction.run, event)) {
                    dispatchAssetEvent({
                        type: assetEventModule.AssetEventType.openProject,
                        id: asset.id,
                        shouldAutomaticallySwitchPage: false,
                        runInBackground: true,
                    })
                } else if (
                    !isRunning &&
                    eventModule.isSingleClick(event) &&
                    (selected ||
                        shortcuts.matchesMouseAction(shortcutsModule.MouseAction.editName, event))
                ) {
                    setRowState(oldRowState => ({
                        ...oldRowState,
                        isEditingName: true,
                    }))
                }
            }}
        >
            {!canExecute ? (
                <SvgMask src={NetworkIcon} className="m-1" />
            ) : (
                <ProjectIcon
                    keyProp={item.key}
                    item={asset}
                    setItem={setAsset}
                    assetEvents={assetEvents}
                    doOpenManually={doOpenManually}
                    openIde={switchPage => {
                        doOpenIde(asset, setAsset, switchPage)
                    }}
                    onClose={() => {
                        doCloseIde(asset)
                    }}
                />
            )}
            <EditableSpan
                editable={rowState.isEditingName}
                onSubmit={async newTitle => {
                    setRowState(oldRowState => ({
                        ...oldRowState,
                        isEditingName: false,
                    }))
                    if (newTitle !== asset.title) {
                        const oldTitle = asset.title
                        setAsset(oldItem => ({ ...oldItem, title: newTitle }))
                        try {
                            await doRename(newTitle)
                        } catch {
                            setAsset(oldItem => ({ ...oldItem, title: oldTitle }))
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
                className={`bg-transparent grow leading-170 h-6 px-2 py-px ${
                    rowState.isEditingName
                        ? 'cursor-text'
                        : canExecute && !isOtherUserUsingProject
                        ? 'cursor-pointer'
                        : ''
                }`}
            >
                {asset.title}
            </EditableSpan>
        </div>
    )
}
