/** @file The directory header bar and directory item listing. */
import * as React from 'react'

import * as appUtils from '#/appUtils'

import * as eventCallback from '#/hooks/eventCallbackHooks'
import * as navigateHooks from '#/hooks/navigateHooks'
import * as searchParamsState from '#/hooks/searchParamsStateHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as textProvider from '#/providers/TextProvider'

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

import * as aria from '#/components/aria'
import type * as spinner from '#/components/Spinner'
import UnstyledButton from '#/components/UnstyledButton'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'
import * as projectManager from '#/services/ProjectManager'

import * as array from '#/utilities/array'
import type AssetQuery from '#/utilities/AssetQuery'
import type AssetTreeNode from '#/utilities/AssetTreeNode'
import type * as colorModule from '#/utilities/color'
import * as download from '#/utilities/download'
import * as github from '#/utilities/github'
import LocalStorage from '#/utilities/LocalStorage'
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
  readonly backend: Backend
  readonly rootDirectory: backendModule.SmartDirectory | null
  readonly initialProjectName: string | null
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
    project: backendModule.SmartProject,
    setProject: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>,
    switchPage: boolean
  ) => void
  readonly doCloseEditor: (project: backendModule.ProjectAsset) => void
}

/** Contains directory path and directory contents (projects, folders, secrets and files). */
export default function Drive(props: DriveProps) {
  const { supportsLocalBackend, backend, hidden, hideRows, initialProjectName } = props
  const { query, setQuery, labels, setLabels, setSuggestions, projectStartupInfo } = props
  const { assetListEvents, dispatchAssetListEvent, assetEvents, dispatchAssetEvent } = props
  const { setAssetPanelProps, doOpenEditor, doCloseEditor } = props
  const { rootDirectory, setIsAssetPanelTemporarilyVisible } = props

  const navigate = navigateHooks.useNavigate()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { type: sessionType, user } = authProvider.useNonPartialUserSession()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const { getText } = textProvider.useText()
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
  const targetDirectoryNodeRef = React.useRef<AssetTreeNode<backendModule.SmartDirectory> | null>(
    null
  )
  const isCloud = backend.type === backendModule.BackendType.remote
  const isEnabled = user?.value.isEnabled ?? false
  const status =
    !isCloud && didLoadingProjectManagerFail
      ? DriveStatus.noProjectManager
      : isCloud && sessionType === authProvider.UserSessionType.offline
        ? DriveStatus.offline
        : isCloud && !isEnabled
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
      if (isCloud && isEnabled) {
        setLabels(await backend.listTags())
      }
    })()
  }, [backend, isCloud, isEnabled, /* should never change */ setLabels])

  const doUploadFiles = React.useCallback(
    (files: File[]) => {
      if (isCloud && user == null) {
        // This should never happen, however display a nice error message in case it does.
        toastAndLog('offlineUploadFilesError')
      } else if (rootDirectory != null) {
        dispatchAssetListEvent({
          type: AssetListEventType.uploadFiles,
          parentKey: targetDirectoryNodeRef.current?.key ?? rootDirectory.value.id,
          parent: targetDirectoryNodeRef.current?.item ?? rootDirectory,
          files,
        })
      }
    },
    [isCloud, user, rootDirectory, toastAndLog, /* should never change */ dispatchAssetListEvent]
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
      const parent = targetDirectoryNodeRef.current?.item ?? rootDirectory
      const parentKey = targetDirectoryNodeRef.current?.key ?? rootDirectory?.value.id
      if (parent != null && parentKey != null) {
        dispatchAssetListEvent({
          type: AssetListEventType.newProject,
          parent,
          parentKey,
          templateId,
          templateName,
          onSpinnerStateChange,
        })
      }
    },
    [rootDirectory, /* should never change */ dispatchAssetListEvent]
  )

  const doCreateDirectory = React.useCallback(() => {
    const parent = targetDirectoryNodeRef.current?.item ?? rootDirectory
    const parentKey = targetDirectoryNodeRef.current?.key ?? rootDirectory?.value.id
    if (parent != null && parentKey != null) {
      dispatchAssetListEvent({ type: AssetListEventType.newFolder, parent, parentKey })
    }
  }, [rootDirectory, /* should never change */ dispatchAssetListEvent])

  const doCreateLabel = React.useCallback(
    async (value: string, color: colorModule.LChColor) => {
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
    [backend, toastAndLog, /* should never change */ setLabels]
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
      toastAndLog,
      /* should never change */ setQuery,
      /* should never change */ dispatchAssetEvent,
      /* should never change */ setLabels,
    ]
  )

  const doCreateSecret = React.useCallback(
    (name: string, value: string) => {
      const parent = targetDirectoryNodeRef.current?.item ?? rootDirectory
      const parentKey = targetDirectoryNodeRef.current?.key ?? rootDirectory?.value.id
      if (parent != null && parentKey != null) {
        dispatchAssetListEvent({
          type: AssetListEventType.newSecret,
          parent,
          parentKey,
          name,
          value,
        })
      }
    },
    [rootDirectory, /* should never change */ dispatchAssetListEvent]
  )

  const doCreateDataLink = React.useCallback(
    (name: string, value: unknown) => {
      const parent = targetDirectoryNodeRef.current?.item ?? rootDirectory
      const parentKey = targetDirectoryNodeRef.current?.key ?? rootDirectory?.value.id
      if (parent != null && parentKey != null) {
        dispatchAssetListEvent({
          type: AssetListEventType.newDataLink,
          parent,
          parentKey,
          name,
          value,
        })
      }
    },
    [rootDirectory, /* should never change */ dispatchAssetListEvent]
  )

  switch (status) {
    case DriveStatus.offline: {
      return (
        <div className={`grid grow place-items-center ${hidden ? 'hidden' : ''}`}>
          <div className="flex flex-col gap-status-page text-center text-base">
            <div>{getText('youAreNotLoggedIn')}</div>
            <UnstyledButton
              className="button self-center bg-help text-white"
              onPress={() => {
                navigate(appUtils.LOGIN_PATH)
              }}
            >
              {getText('login')}
            </UnstyledButton>
          </div>
        </div>
      )
    }
    case DriveStatus.noProjectManager: {
      return (
        <div className={`grid grow place-items-center ${hidden ? 'hidden' : ''}`}>
          <div className="flex flex-col gap-status-page text-center text-base">
            {getText('couldNotConnectToPM')}
          </div>
        </div>
      )
    }
    case DriveStatus.notEnabled: {
      return (
        <div className={`grid grow place-items-center ${hidden ? 'hidden' : ''}`}>
          <div className="flex flex-col gap-status-page text-center text-base">
            {getText('upgradeToUseCloud')}
            <a className="button self-center bg-help text-white" href="https://enso.org/pricing">
              {getText('upgrade')}
            </a>
            {!supportsLocalBackend && (
              <UnstyledButton
                className="button self-center bg-help text-white"
                onPress={async () => {
                  const downloadUrl = await github.getDownloadUrl()
                  if (downloadUrl == null) {
                    toastAndLog('noAppDownloadError')
                  } else {
                    download.download(downloadUrl)
                  }
                }}
              >
                {getText('downloadFreeEdition')}
              </UnstyledButton>
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
          <div className="flex flex-col items-start gap-icons self-start">
            <aria.Heading
              level={1}
              className="h-heading px-heading-x py-heading-y text-xl font-bold leading-snug"
            >
              {isCloud ? getText('cloudDrive') : getText('localDrive')}
            </aria.Heading>
            <DriveBar
              category={category}
              isCloud={isCloud}
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
            {isCloud && (
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
            {rootDirectory != null && (
              <AssetsTable
                isCloud={isCloud}
                rootDirectory={rootDirectory}
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
                assetEvents={assetEvents}
                dispatchAssetEvent={dispatchAssetEvent}
                assetListEvents={assetListEvents}
                dispatchAssetListEvent={dispatchAssetListEvent}
                setAssetPanelProps={setAssetPanelProps}
                setIsAssetPanelTemporarilyVisible={setIsAssetPanelTemporarilyVisible}
                targetDirectoryNodeRef={targetDirectoryNodeRef}
                doOpenEditor={doOpenEditor}
                doCloseEditor={doCloseEditor}
                doCreateLabel={doCreateLabel}
              />
            )}
          </div>
        </div>
      )
    }
  }
}
