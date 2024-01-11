/** @file The directory header bar and directory item listing. */
import * as React from 'react'

import * as common from 'enso-common'

import * as appUtils from '#/appUtils'
import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'
import type * as assetListEvent from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'
import * as navigateHooks from '#/hooks/navigateHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'
import type * as assetSearchBar from '#/layouts/dashboard/assetSearchBar'
import type * as assetSettingsPanel from '#/layouts/dashboard/AssetSettingsPanel'
import AssetsTable from '#/layouts/dashboard/AssetsTable'
import CategorySwitcher from '#/layouts/dashboard/CategorySwitcher'
import Category from '#/layouts/dashboard/CategorySwitcher/Category'
import DriveBar from '#/layouts/dashboard/DriveBar'
import Labels from '#/layouts/dashboard/Labels'
import * as pageSwitcher from '#/layouts/dashboard/PageSwitcher'
import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as backendModule from '#/services/backend'
import type * as assetQuery from '#/utilities/assetQuery'
import * as github from '#/utilities/github'
import * as localStorageModule from '#/utilities/localStorage'
import * as uniqueString from '#/utilities/uniqueString'

import type * as spinner from '#/components/Spinner'

// =============
// === Drive ===
// =============

/** Props for a {@link Drive}. */
export interface DriveProps {
    supportsLocalBackend: boolean
    hidden: boolean
    page: pageSwitcher.Page
    initialProjectName: string | null
    /** These events will be dispatched the next time the assets list is refreshed, rather than
     * immediately. */
    queuedAssetEvents: assetEvent.AssetEvent[]
    assetListEvents: assetListEvent.AssetListEvent[]
    dispatchAssetListEvent: (directoryEvent: assetListEvent.AssetListEvent) => void
    assetEvents: assetEvent.AssetEvent[]
    dispatchAssetEvent: (directoryEvent: assetEvent.AssetEvent) => void
    query: assetQuery.AssetQuery
    setQuery: React.Dispatch<React.SetStateAction<assetQuery.AssetQuery>>
    labels: backendModule.Label[]
    setLabels: React.Dispatch<React.SetStateAction<backendModule.Label[]>>
    setSuggestions: (suggestions: assetSearchBar.Suggestion[]) => void
    projectStartupInfo: backendModule.ProjectStartupInfo | null
    setAssetSettingsPanelProps: React.Dispatch<
        React.SetStateAction<assetSettingsPanel.AssetSettingsPanelRequiredProps | null>
    >
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
    const { supportsLocalBackend, hidden, page, initialProjectName, queuedAssetEvents } = props
    const { query, setQuery, labels, setLabels, setSuggestions, projectStartupInfo } = props
    const { assetListEvents, dispatchAssetListEvent, assetEvents, dispatchAssetEvent } = props
    const { setAssetSettingsPanelProps, doOpenEditor, doCloseEditor } = props
    const { loadingProjectManagerDidFail, isListingRemoteDirectoryWhileOffline } = props
    const { isListingLocalDirectoryAndWillFail, isListingRemoteDirectoryAndWillFail } = props

    const navigate = navigateHooks.useNavigate()
    const toastAndLog = toastAndLogHooks.useToastAndLog()
    const { organization } = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const { localStorage } = localStorageProvider.useLocalStorage()
    const { modalRef } = modalProvider.useModalRef()
    const [isFileBeingDragged, setIsFileBeingDragged] = React.useState(false)
    const [category, setCategory] = React.useState(
        () => localStorage.get(localStorageModule.LocalStorageKey.driveCategory) ?? Category.home
    )
    const [newLabelNames, setNewLabelNames] = React.useState(new Set<backendModule.LabelName>())
    const [deletedLabelNames, setDeletedLabelNames] = React.useState(
        new Set<backendModule.LabelName>()
    )
    const allLabels = React.useMemo(
        () => new Map(labels.map(label => [label.value, label])),
        [labels]
    )
    const rootDirectoryId = React.useMemo(
        () => organization?.rootDirectoryId ?? backendModule.DirectoryId(''),
        [organization]
    )
    const isCloud = backend.type === backendModule.BackendType.remote

    React.useEffect(() => {
        if (modalRef.current != null) {
            setIsFileBeingDragged(false)
        }
    }, [/* should never change */ modalRef])

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
    }, [backend, organization?.isEnabled, /* should never change */ setLabels])

    const doUploadFiles = React.useCallback(
        (files: File[]) => {
            if (backend.type !== backendModule.BackendType.local && organization == null) {
                // This should never happen, however display a nice error message in case it does.
                toastAndLog('Files cannot be uploaded while offline')
            } else {
                dispatchAssetListEvent({
                    type: AssetListEventType.uploadFiles,
                    parentKey: rootDirectoryId,
                    parentId: rootDirectoryId,
                    files,
                })
            }
        },
        [
            backend,
            organization,
            rootDirectoryId,
            toastAndLog,
            /* should never change */ dispatchAssetListEvent,
        ]
    )

    const doCreateProject = React.useCallback(
        (
            templateId: string | null,
            onSpinnerStateChange?: (state: spinner.SpinnerState) => void
        ) => {
            dispatchAssetListEvent({
                type: AssetListEventType.newProject,
                parentKey: rootDirectoryId,
                parentId: rootDirectoryId,
                templateId: templateId ?? null,
                onSpinnerStateChange: onSpinnerStateChange ?? null,
            })
        },
        [rootDirectoryId, /* should never change */ dispatchAssetListEvent]
    )

    const doCreateDirectory = React.useCallback(() => {
        dispatchAssetListEvent({
            type: AssetListEventType.newFolder,
            parentKey: rootDirectoryId,
            parentId: rootDirectoryId,
        })
    }, [rootDirectoryId, /* should never change */ dispatchAssetListEvent])

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
            } catch (error) {
                toastAndLog(null, error)
                setLabels(oldLabels =>
                    oldLabels.filter(oldLabel => oldLabel.id !== placeholderLabel.id)
                )
            }
            setNewLabelNames(
                labelNames =>
                    new Set([...labelNames].filter(labelName => labelName !== newLabelName))
            )
        },
        [backend, /* should never change */ toastAndLog, /* should never change */ setLabels]
    )

    const doDeleteLabel = React.useCallback(
        async (id: backendModule.TagId, value: backendModule.LabelName) => {
            setDeletedLabelNames(oldNames => new Set([...oldNames, value]))
            setQuery(oldQuery => oldQuery.deleteFromEveryTerm({ labels: [value] }))
            try {
                await backend.deleteTag(id, value)
                dispatchAssetEvent({
                    type: AssetEventType.deleteLabel,
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
            /* should never change */ setQuery,
            /* should never change */ dispatchAssetEvent,
            /* should never change */ toastAndLog,
            /* should never change */ setLabels,
        ]
    )

    const doCreateDataConnector = React.useCallback(
        (name: string, value: string) => {
            dispatchAssetListEvent({
                type: AssetListEventType.newDataConnector,
                parentKey: rootDirectoryId,
                parentId: rootDirectoryId,
                name,
                value,
            })
        },
        [rootDirectoryId, /* should never change */ dispatchAssetListEvent]
    )

    React.useEffect(() => {
        const onDragEnter = (event: DragEvent) => {
            if (
                modalRef.current == null &&
                page === pageSwitcher.Page.drive &&
                category === Category.home &&
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
                <div className="text-base text-center">You are not logged in.</div>
                <button
                    className="text-base text-white bg-help rounded-full self-center leading-170 h-8 py-px w-16"
                    onClick={() => {
                        navigate(appUtils.LOGIN_PATH)
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
            <div className="flex flex-col gap-4 text-base text-center">
                Upgrade your plan to use {common.PRODUCT_NAME} Cloud.
                <a
                    className="block self-center whitespace-nowrap text-base text-white bg-help rounded-full leading-170 h-8 py-px px-2 w-min"
                    href="https://enso.org/pricing"
                >
                    Upgrade
                </a>
                {!supportsLocalBackend && (
                    <button
                        className="block self-center whitespace-nowrap text-base text-white bg-help rounded-full leading-170 h-8 py-px px-2 w-min"
                        onClick={async () => {
                            const downloadUrl = await github.getDownloadUrl()
                            if (downloadUrl == null) {
                                toastAndLog('Could not find a download link for the current OS')
                            } else {
                                window.open(downloadUrl, '_blank')
                            }
                        }}
                    >
                        Download Free Edition
                    </button>
                )}
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
                            query={query}
                            setQuery={setQuery}
                            doCreateLabel={doCreateLabel}
                            doDeleteLabel={doDeleteLabel}
                            newLabelNames={newLabelNames}
                            deletedLabelNames={deletedLabelNames}
                        />
                    </div>
                )}
                <AssetsTable
                    query={query}
                    setQuery={setQuery}
                    category={category}
                    allLabels={allLabels}
                    setSuggestions={setSuggestions}
                    initialProjectName={initialProjectName}
                    projectStartupInfo={projectStartupInfo}
                    deletedLabelNames={deletedLabelNames}
                    queuedAssetEvents={queuedAssetEvents}
                    assetEvents={assetEvents}
                    dispatchAssetEvent={dispatchAssetEvent}
                    assetListEvents={assetListEvents}
                    dispatchAssetListEvent={dispatchAssetListEvent}
                    setAssetSettingsPanelProps={setAssetSettingsPanelProps}
                    doOpenIde={doOpenEditor}
                    doCloseIde={doCloseEditor}
                    doCreateLabel={doCreateLabel}
                    loadingProjectManagerDidFail={loadingProjectManagerDidFail}
                    isListingRemoteDirectoryWhileOffline={isListingRemoteDirectoryWhileOffline}
                    isListingLocalDirectoryAndWillFail={isListingLocalDirectoryAndWillFail}
                    isListingRemoteDirectoryAndWillFail={isListingRemoteDirectoryAndWillFail}
                />
            </div>
            {isFileBeingDragged && organization != null && isCloud ? (
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
                            type: AssetListEventType.uploadFiles,
                            parentKey: rootDirectoryId,
                            parentId: rootDirectoryId,
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
