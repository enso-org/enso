/** @file The directory header bar and directory item listing. */
import * as React from 'react'

import * as common from 'enso-common'

import * as appUtils from '#/appUtils'

import * as eventCallback from '#/hooks/eventCallback'
import * as navigateHooks from '#/hooks/navigateHooks'
import * as searchParamsState from '#/hooks/searchParamsState'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'
import type * as assetListEvent from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'

import type * as assetPanel from '#/layouts/AssetPanel'
import type * as assetSearchBar from '#/layouts/AssetSearchBar'
import AssetsTable from '#/layouts/AssetsTable'
import CategorySwitcher from '#/layouts/CategorySwitcher'
import Category from '#/layouts/CategorySwitcher/Category'
import DriveBar from '#/layouts/DriveBar'
import Labels from '#/layouts/Labels'

import type * as spinner from '#/components/Spinner'

import * as backendModule from '#/services/Backend'

import * as array from '#/utilities/array'
import type AssetQuery from '#/utilities/AssetQuery'
import * as download from '#/utilities/download'
import * as github from '#/utilities/github'
import LocalStorage from '#/utilities/LocalStorage'
import * as projectManager from '#/utilities/ProjectManager'
import * as uniqueString from '#/utilities/uniqueString'

// ============================
// === Global configuration ===
// ============================

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly driveCategory: Category
  }
}

const CATEGORIES = Object.values(Category)
LocalStorage.registerKey('driveCategory', {
  tryParse: value => (array.includes(CATEGORIES, value) ? value : null),
})

// ===================
// === DriveStatus ===
// ===================

/** The predicted status of project listing. This is used to avoid sending requests to the backend
 * if it is already known that the request will fail. */
enum DriveStatus {
  /** No errors predicted. The request may still error because of an issue in the backend. */
  ok = 'ok',
  /** Trying to use the remote backend when offline. The network request will fail. */
  offline = 'offline',
  /** The user does not have an active plan, and therefore has no access to the remote backend. */
  notEnabled = 'not-enabled',
  /** The connection to the Project Manager timed out. This may happen if the Project Manager
   * crashed, or was never run in the first place. */
  noProjectManager = 'no-project-manager',
}

// =============
// === Drive ===
// =============

/** Props for a {@link Drive}. */
export interface DriveProps {
  readonly supportsLocalBackend: boolean
  readonly hidden: boolean
  readonly hideRows: boolean
  readonly initialProjectName: string | null
  /** These events will be dispatched the next time the assets list is refreshed, rather than
   * immediately. */
  readonly queuedAssetEvents: assetEvent.AssetEvent[]
  readonly assetListEvents: assetListEvent.AssetListEvent[]
  readonly dispatchAssetListEvent: (directoryEvent: assetListEvent.AssetListEvent) => void
  readonly assetEvents: assetEvent.AssetEvent[]
  readonly dispatchAssetEvent: (directoryEvent: assetEvent.AssetEvent) => void
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly labels: backendModule.Label[]
  readonly setLabels: React.Dispatch<React.SetStateAction<backendModule.Label[]>>
  readonly setSuggestions: (suggestions: assetSearchBar.Suggestion[]) => void
  readonly projectStartupInfo: backendModule.ProjectStartupInfo | null
  readonly setAssetPanelProps: (props: assetPanel.AssetPanelRequiredProps | null) => void
  readonly setIsAssetPanelTemporarilyVisible: (visible: boolean) => void
  readonly doCreateProject: (templateId: string | null) => void
  readonly doOpenEditor: (
    project: backendModule.ProjectAsset,
    setProject: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>,
    switchPage: boolean
  ) => void
  readonly doCloseEditor: (project: backendModule.ProjectAsset) => void
}

/** Contains directory path and directory contents (projects, folders, secrets and files). */
export default function Drive(props: DriveProps) {
  const { supportsLocalBackend, hidden, hideRows, initialProjectName, queuedAssetEvents } = props
  const { query, setQuery, labels, setLabels, setSuggestions, projectStartupInfo } = props
  const { assetListEvents, dispatchAssetListEvent, assetEvents, dispatchAssetEvent } = props
  const { setAssetPanelProps, doOpenEditor, doCloseEditor } = props
  const { setIsAssetPanelTemporarilyVisible } = props

  const navigate = navigateHooks.useNavigate()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { type: sessionType, user } = authProvider.useNonPartialUserSession()
  const { backend } = backendProvider.useBackend()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const [canDownload, setCanDownload] = React.useState(false)
  const [didLoadingProjectManagerFail, setDidLoadingProjectManagerFail] = React.useState(false)
  const [category, setCategory] = searchParamsState.useSearchParamsState(
    'driveCategory',
    () => localStorage.get('driveCategory') ?? Category.home,
    (value): value is Category => array.includes(Object.values(Category), value)
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
    () => user?.rootDirectoryId ?? backendModule.DirectoryId(''),
    [user]
  )
  const isCloud = backend.type === backendModule.BackendType.remote
  const status =
    !isCloud && didLoadingProjectManagerFail
      ? DriveStatus.noProjectManager
      : isCloud && sessionType === authProvider.UserSessionType.offline
        ? DriveStatus.offline
        : isCloud && user?.isEnabled !== true
          ? DriveStatus.notEnabled
          : DriveStatus.ok

  const onSetCategory = eventCallback.useEventCallback((value: Category) => {
    setCategory(value)
    localStorage.set('driveCategory', value)
  })

  React.useEffect(() => {
    const onProjectManagerLoadingFailed = () => {
      setDidLoadingProjectManagerFail(true)
    }
    document.addEventListener(
      projectManager.ProjectManagerEvents.loadingFailed,
      onProjectManagerLoadingFailed
    )
    return () => {
      document.removeEventListener(
        projectManager.ProjectManagerEvents.loadingFailed,
        onProjectManagerLoadingFailed
      )
    }
  }, [])

  React.useEffect(() => {
    void (async () => {
      if (backend.type !== backendModule.BackendType.local && user?.isEnabled === true) {
        setLabels(await backend.listTags())
      }
    })()
  }, [backend, user?.isEnabled, /* should never change */ setLabels])

  const doUploadFiles = React.useCallback(
    (files: File[]) => {
      if (backend.type !== backendModule.BackendType.local && user == null) {
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
    [backend, user, rootDirectoryId, toastAndLog, /* should never change */ dispatchAssetListEvent]
  )

  const doEmptyTrash = React.useCallback(() => {
    dispatchAssetListEvent({ type: AssetListEventType.emptyTrash })
  }, [/* should never change */ dispatchAssetListEvent])

  const doCreateProject = React.useCallback(
    (
      templateId: string | null = null,
      templateName: string | null = null,
      onSpinnerStateChange: ((state: spinner.SpinnerState) => void) | null = null
    ) => {
      dispatchAssetListEvent({
        type: AssetListEventType.newProject,
        parentKey: rootDirectoryId,
        parentId: rootDirectoryId,
        templateId,
        templateName,
        onSpinnerStateChange,
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
          oldLabels.map(oldLabel => (oldLabel.id === placeholderLabel.id ? newLabel : oldLabel))
        )
      } catch (error) {
        toastAndLog(null, error)
        setLabels(oldLabels => oldLabels.filter(oldLabel => oldLabel.id !== placeholderLabel.id))
      }
      setNewLabelNames(
        labelNames => new Set([...labelNames].filter(labelName => labelName !== newLabelName))
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

  const doCreateSecret = React.useCallback(
    (name: string, value: string) => {
      dispatchAssetListEvent({
        type: AssetListEventType.newSecret,
        parentKey: rootDirectoryId,
        parentId: rootDirectoryId,
        name,
        value,
      })
    },
    [rootDirectoryId, /* should never change */ dispatchAssetListEvent]
  )

  const doCreateDataLink = React.useCallback(
    (name: string, value: unknown) => {
      dispatchAssetListEvent({
        type: AssetListEventType.newDataLink,
        parentKey: rootDirectoryId,
        parentId: rootDirectoryId,
        name,
        value,
      })
    },
    [rootDirectoryId, /* should never change */ dispatchAssetListEvent]
  )

  switch (status) {
    case DriveStatus.offline: {
      return (
        <div className={`grid grow place-items-center ${hidden ? 'hidden' : ''}`}>
          <div className="flex flex-col gap-status-page text-center text-base">
            <div>You are not logged in.</div>
            <button
              className="button self-center bg-help text-white"
              onClick={() => {
                navigate(appUtils.LOGIN_PATH)
              }}
            >
              Login
            </button>
          </div>
        </div>
      )
    }
    case DriveStatus.noProjectManager: {
      return (
        <div className={`grid grow place-items-center ${hidden ? 'hidden' : ''}`}>
          <div className="flex flex-col gap-status-page text-center text-base">
            Could not connect to the Project Manager. Please try restarting {common.PRODUCT_NAME},
            or manually launching the Project Manager.
          </div>
        </div>
      )
    }
    case DriveStatus.notEnabled: {
      return (
        <div className={`grid grow place-items-center ${hidden ? 'hidden' : ''}`}>
          <div className="flex flex-col gap-status-page text-center text-base">
            Upgrade your plan to use {common.PRODUCT_NAME} Cloud.
            <a className="button self-center bg-help text-white" href="https://enso.org/pricing">
              Upgrade
            </a>
            {!supportsLocalBackend && (
              <button
                className="button self-center bg-help text-white"
                onClick={async () => {
                  const downloadUrl = await github.getDownloadUrl()
                  if (downloadUrl == null) {
                    toastAndLog('Could not find a download link for the current OS')
                  } else {
                    download.download(downloadUrl)
                  }
                }}
              >
                Download Free Edition
              </button>
            )}
          </div>
        </div>
      )
    }
    case DriveStatus.ok: {
      return (
        <div
          data-testid="drive-view"
          className={`flex flex-1 flex-col gap-drive-heading overflow-hidden px-page-x ${
            hidden ? 'hidden' : ''
          }`}
        >
          <div className="flex flex-col gap-icons self-start">
            <h1 className="h-heading px-heading-x py-heading-y text-xl font-bold leading-snug">
              {backend.type === backendModule.BackendType.remote ? 'Cloud Drive' : 'Local Drive'}
            </h1>
            <DriveBar
              category={category}
              canDownload={canDownload}
              doEmptyTrash={doEmptyTrash}
              doCreateProject={doCreateProject}
              doUploadFiles={doUploadFiles}
              doCreateDirectory={doCreateDirectory}
              doCreateSecret={doCreateSecret}
              doCreateDataLink={doCreateDataLink}
              dispatchAssetEvent={dispatchAssetEvent}
            />
          </div>
          <div className="flex flex-1 gap-drive overflow-hidden">
            {backend.type === backendModule.BackendType.remote && (
              <div className="flex w-drive-sidebar flex-col gap-drive-sidebar py-drive-sidebar-y">
                <CategorySwitcher
                  category={category}
                  setCategory={onSetCategory}
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
              hidden={hidden}
              hideRows={hideRows}
              query={query}
              setQuery={setQuery}
              setCanDownload={setCanDownload}
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
              setAssetPanelProps={setAssetPanelProps}
              setIsAssetPanelTemporarilyVisible={setIsAssetPanelTemporarilyVisible}
              doOpenEditor={doOpenEditor}
              doCloseEditor={doCloseEditor}
              doCreateLabel={doCreateLabel}
            />
          </div>
        </div>
      )
    }
  }
}
