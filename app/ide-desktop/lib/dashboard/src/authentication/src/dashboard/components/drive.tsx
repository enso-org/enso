/** @file The directory header bar and directory item listing. */
import * as React from 'react'

import * as common from 'enso-common'

import * as assetEventModule from '../events/assetEvent'
import * as assetListEventModule from '../events/assetListEvent'
import * as authProvider from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as hooks from '../../hooks'
import * as identity from '../identity'
import * as localStorageModule from '../localStorage'
import * as localStorageProvider from '../../providers/localStorage'
import * as modalProvider from '../../providers/modal'
import * as uniqueString from '../../uniqueString'

import * as app from '../../components/app'
import * as pageSwitcher from './pageSwitcher'
import type * as spinner from './spinner'
import CategorySwitcher, * as categorySwitcher from './categorySwitcher'
import AssetsTable from './assetsTable'
import DriveBar from './driveBar'
import Labels from './labels'

// =============
// === Drive ===
// =============

/** Props for a {@link Drive}. */
export interface DriveProps {
    page: pageSwitcher.Page
    hidden: boolean
    initialProjectName: string | null
    /** These events will be dispatched the next time the assets list is refreshed, rather than
     * immediately. */
    queuedAssetEvents: assetEventModule.AssetEvent[]
    assetListEvents: assetListEventModule.AssetListEvent[]
    dispatchAssetListEvent: (directoryEvent: assetListEventModule.AssetListEvent) => void
    assetEvents: assetEventModule.AssetEvent[]
    dispatchAssetEvent: (directoryEvent: assetEventModule.AssetEvent) => void
    query: string
    projectStartupInfo: backendModule.ProjectStartupInfo | null
    doCreateProject: (templateId: string | null) => void
    doOpenEditor: (
        project: backendModule.ProjectAsset,
        setProject: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>,
        switchPage: boolean
    ) => void
    doCloseEditor: (project: backendModule.ProjectAsset) => void
    loadingProjectManagerDidFail: boolean
    isListingRemoteDirectoryWhileOffline: boolean
    isListingLocalDirectoryAndWillFail: boolean
    isListingRemoteDirectoryAndWillFail: boolean
}

/** Contains directory path and directory contents (projects, folders, secrets and files). */
export default function Drive(props: DriveProps) {
    const {
        page,
        hidden,
        initialProjectName,
        queuedAssetEvents,
        query,
        projectStartupInfo,
        assetListEvents,
        dispatchAssetListEvent,
        assetEvents,
        dispatchAssetEvent,
        doOpenEditor,
        doCloseEditor,
        loadingProjectManagerDidFail,
        isListingRemoteDirectoryWhileOffline,
        isListingLocalDirectoryAndWillFail,
        isListingRemoteDirectoryAndWillFail,
    } = props
    const navigate = hooks.useNavigate()
    const { organization } = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const { localStorage } = localStorageProvider.useLocalStorage()
    const { modal } = modalProvider.useModal()
    const { modalRef } = modalProvider.useModalRef()
    const toastAndLog = hooks.useToastAndLog()
    const [isFileBeingDragged, setIsFileBeingDragged] = React.useState(false)
    const [category, setCategory] = React.useState(
        () =>
            localStorage.get(localStorageModule.LocalStorageKey.driveCategory) ??
            categorySwitcher.Category.home
    )
    const [labels, setLabels] = React.useState<backendModule.Label[]>([])
    const [currentLabels, setCurrentLabels] = React.useState<backendModule.LabelName[] | null>(null)
    const [newLabelNames, setNewLabelNames] = React.useState(new Set<backendModule.LabelName>())
    const [deletedLabelNames, setDeletedLabelNames] = React.useState(
        new Set<backendModule.LabelName>()
    )
    const allLabels = React.useMemo(
        () => new Map(labels.map(label => [label.value, label])),
        [labels]
    )

    React.useEffect(() => {
        if (modal != null) {
            setIsFileBeingDragged(false)
        }
    }, [modal])

    React.useEffect(() => {
        const onBlur = () => {
            setIsFileBeingDragged(false)
        }
        window.addEventListener('blur', onBlur)
        return () => {
            window.removeEventListener('blur', onBlur)
        }
    }, [])

    React.useEffect(() => {
        void (async () => {
            if (
                backend.type !== backendModule.BackendType.local &&
                organization?.isEnabled === true
            ) {
                setLabels(await backend.listTags())
            }
        })()
    }, [backend, organization?.isEnabled])

    const doUploadFiles = React.useCallback(
        (files: File[]) => {
            if (backend.type !== backendModule.BackendType.local && organization == null) {
                // This should never happen, however display a nice error message in case it does.
                toastAndLog('Files cannot be uploaded while offline')
            } else {
                dispatchAssetListEvent({
                    type: assetListEventModule.AssetListEventType.uploadFiles,
                    parentKey: null,
                    parentId: null,
                    files,
                })
            }
        },
        [backend, organization, toastAndLog, /* should never change */ dispatchAssetListEvent]
    )

    const doCreateProject = React.useCallback(
        (
            templateId: string | null,
            onSpinnerStateChange?: (state: spinner.SpinnerState) => void
        ) => {
            dispatchAssetListEvent({
                type: assetListEventModule.AssetListEventType.newProject,
                parentKey: null,
                parentId: null,
                templateId: templateId ?? null,
                onSpinnerStateChange: onSpinnerStateChange ?? null,
            })
        },
        [/* should never change */ dispatchAssetListEvent]
    )

    const doCreateDirectory = React.useCallback(() => {
        dispatchAssetListEvent({
            type: assetListEventModule.AssetListEventType.newFolder,
            parentKey: null,
            parentId: null,
        })
    }, [/* should never change */ dispatchAssetListEvent])

    const doCreateLabel = React.useCallback(
        async (value: string, color: backendModule.LChColor) => {
            const newLabelName = backendModule.LabelName(value)
            const placeholderLabel: backendModule.Label = {
                id: backendModule.TagId(uniqueString.uniqueString()),
                value: newLabelName,
                color,
            }
            setNewLabelNames(labelNames => new Set([...labelNames, newLabelName]))
            setLabels(oldLabels => [...oldLabels, placeholderLabel])
            try {
                const newLabel = await backend.createTag({ value, color })
                setLabels(oldLabels =>
                    oldLabels.map(oldLabel =>
                        oldLabel.id === placeholderLabel.id ? newLabel : oldLabel
                    )
                )
                setCurrentLabels(oldLabels => {
                    let found = identity.identity<boolean>(false)
                    const newLabels =
                        oldLabels?.map(oldLabel => {
                            if (oldLabel === placeholderLabel.value) {
                                found = true
                                return newLabel.value
                            } else {
                                return oldLabel
                            }
                        }) ?? null
                    return found ? newLabels : oldLabels
                })
            } catch (error) {
                toastAndLog(null, error)
                setLabels(oldLabels =>
                    oldLabels.filter(oldLabel => oldLabel.id !== placeholderLabel.id)
                )
                setCurrentLabels(oldLabels => {
                    let found = identity.identity<boolean>(false)
                    const newLabels = (oldLabels ?? []).filter(oldLabel => {
                        if (oldLabel === placeholderLabel.value) {
                            found = true
                            return false
                        } else {
                            return true
                        }
                    })
                    return found ? (newLabels.length === 0 ? null : newLabels) : oldLabels
                })
            }
            setNewLabelNames(
                labelNames =>
                    new Set([...labelNames].filter(labelName => labelName !== newLabelName))
            )
        },
        [backend, /* should never change */ toastAndLog]
    )

    const doDeleteLabel = React.useCallback(
        async (id: backendModule.TagId, value: backendModule.LabelName) => {
            setDeletedLabelNames(oldNames => new Set([...oldNames, value]))
            setCurrentLabels(oldLabels => {
                let found = identity.identity<boolean>(false)
                const newLabels = oldLabels?.filter(oldLabel => {
                    if (oldLabel === value) {
                        found = true
                        return false
                    } else {
                        return true
                    }
                })
                return newLabels != null && newLabels.length > 0
                    ? found
                        ? newLabels
                        : oldLabels
                    : null
            })
            try {
                await backend.deleteTag(id, value)
                dispatchAssetEvent({
                    type: assetEventModule.AssetEventType.deleteLabel,
                    labelName: value,
                })
                setLabels(oldLabels => oldLabels.filter(oldLabel => oldLabel.id !== id))
            } catch (error) {
                toastAndLog(null, error)
            }
            setDeletedLabelNames(
                oldNames => new Set([...oldNames].filter(oldValue => oldValue !== value))
            )
        },
        [
            backend,
            /* should never change */ dispatchAssetEvent,
            /* should never change */ toastAndLog,
        ]
    )

    const doCreateDataConnector = React.useCallback(
        (name: string, value: string) => {
            dispatchAssetListEvent({
                type: assetListEventModule.AssetListEventType.newDataConnector,
                parentKey: null,
                parentId: null,
                name,
                value,
            })
        },
        [/* should never change */ dispatchAssetListEvent]
    )

    React.useEffect(() => {
        const onDragEnter = (event: DragEvent) => {
            if (
                modalRef.current == null &&
                page === pageSwitcher.Page.drive &&
                category === categorySwitcher.Category.home &&
                event.dataTransfer?.types.includes('Files') === true
            ) {
                setIsFileBeingDragged(true)
            }
        }
        document.body.addEventListener('dragenter', onDragEnter)
        return () => {
            document.body.removeEventListener('dragenter', onDragEnter)
        }
    }, [page, category, /* should never change */ modalRef])

    return isListingRemoteDirectoryWhileOffline ? (
        <div className={`grow grid place-items-center mx-2 ${hidden ? 'hidden' : ''}`}>
            <div className="flex flex-col gap-4">
                <div className="text-base text-center">You are not signed in.</div>
                <button
                    className="text-base text-white bg-help rounded-full self-center leading-170 h-8 py-px w-16"
                    onClick={() => {
                        navigate(app.LOGIN_PATH)
                    }}
                >
                    Login
                </button>
            </div>
        </div>
    ) : isListingLocalDirectoryAndWillFail ? (
        <div className={`grow grid place-items-center mx-2 ${hidden ? 'hidden' : ''}`}>
            <div className="text-base text-center">
                Could not connect to the Project Manager. Please try restarting{' '}
                {common.PRODUCT_NAME}, or manually launching the Project Manager.
            </div>
        </div>
    ) : isListingRemoteDirectoryAndWillFail ? (
        <div className={`grow grid place-items-center mx-2 ${hidden ? 'hidden' : ''}`}>
            <div className="text-base text-center">
                We will review your user details and enable the cloud experience for you shortly.
            </div>
        </div>
    ) : (
        <div
            data-testid="drive-view"
            className={`flex flex-col flex-1 overflow-hidden gap-2.5 px-3.25 mt-8 ${
                hidden ? 'hidden' : ''
            }`}
        >
            <div className="flex flex-col self-start gap-3">
                <h1 className="text-xl font-bold h-9.5 pl-1.5">
                    {backend.type === backendModule.BackendType.remote
                        ? 'Cloud Drive'
                        : 'Local Drive'}
                </h1>
                <DriveBar
                    category={category}
                    doCreateProject={doCreateProject}
                    doUploadFiles={doUploadFiles}
                    doCreateDirectory={doCreateDirectory}
                    doCreateDataConnector={doCreateDataConnector}
                    dispatchAssetEvent={dispatchAssetEvent}
                />
            </div>
            <div className="flex flex-1 gap-3 overflow-hidden">
                {backend.type === backendModule.BackendType.remote && (
                    <div className="flex flex-col gap-4 py-1">
                        <CategorySwitcher
                            category={category}
                            setCategory={setCategory}
                            dispatchAssetEvent={dispatchAssetEvent}
                        />
                        <Labels
                            labels={labels}
                            currentLabels={currentLabels}
                            setCurrentLabels={setCurrentLabels}
                            doCreateLabel={doCreateLabel}
                            doDeleteLabel={doDeleteLabel}
                            newLabelNames={newLabelNames}
                            deletedLabelNames={deletedLabelNames}
                        />
                    </div>
                )}
                <AssetsTable
                    query={query}
                    category={category}
                    allLabels={allLabels}
                    currentLabels={currentLabels}
                    initialProjectName={initialProjectName}
                    projectStartupInfo={projectStartupInfo}
                    deletedLabelNames={deletedLabelNames}
                    queuedAssetEvents={queuedAssetEvents}
                    assetEvents={assetEvents}
                    dispatchAssetEvent={dispatchAssetEvent}
                    assetListEvents={assetListEvents}
                    dispatchAssetListEvent={dispatchAssetListEvent}
                    doOpenIde={doOpenEditor}
                    doCloseIde={doCloseEditor}
                    doCreateLabel={doCreateLabel}
                    loadingProjectManagerDidFail={loadingProjectManagerDidFail}
                    isListingRemoteDirectoryWhileOffline={isListingRemoteDirectoryWhileOffline}
                    isListingLocalDirectoryAndWillFail={isListingLocalDirectoryAndWillFail}
                    isListingRemoteDirectoryAndWillFail={isListingRemoteDirectoryAndWillFail}
                />
            </div>
            {isFileBeingDragged &&
            organization != null &&
            backend.type === backendModule.BackendType.remote ? (
                <div
                    className="text-white text-lg fixed w-screen h-screen inset-0 bg-primary bg-opacity-75 backdrop-blur-xs grid place-items-center z-3"
                    onDragLeave={() => {
                        setIsFileBeingDragged(false)
                    }}
                    onDragOver={event => {
                        event.preventDefault()
                    }}
                    onDrop={event => {
                        event.preventDefault()
                        setIsFileBeingDragged(false)
                        dispatchAssetListEvent({
                            type: assetListEventModule.AssetListEventType.uploadFiles,
                            parentKey: null,
                            parentId: null,
                            files: Array.from(event.dataTransfer.files),
                        })
                    }}
                >
                    Drop to upload files.
                </div>
            ) : null}
        </div>
    )
}
