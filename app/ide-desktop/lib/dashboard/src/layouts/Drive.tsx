/** @file The directory header bar and directory item listing. */
import * as React from 'react'

import * as appUtils from '#/appUtils'

import * as eventCallback from '#/hooks/eventCallbackHooks'
import * as navigateHooks from '#/hooks/navigateHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'
import type * as assetListEvent from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'

import type * as assetPanel from '#/layouts/AssetPanel'
import AssetPanel from '#/layouts/AssetPanel'
import type * as assetSearchBar from '#/layouts/AssetSearchBar'
import AssetsTable from '#/layouts/AssetsTable'
import CategorySwitcher from '#/layouts/CategorySwitcher'
import Category, * as categoryModule from '#/layouts/CategorySwitcher/Category'
import DriveBar from '#/layouts/DriveBar'
import Labels from '#/layouts/Labels'

import * as ariaComponents from '#/components/AriaComponents'
import * as result from '#/components/Result'
import UnstyledButton from '#/components/UnstyledButton'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'
import * as projectManager from '#/services/ProjectManager'

import AssetQuery from '#/utilities/AssetQuery'
import type AssetTreeNode from '#/utilities/AssetTreeNode'
import * as download from '#/utilities/download'
import * as github from '#/utilities/github'
import * as uniqueString from '#/utilities/uniqueString'

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
  readonly category: Category
  readonly setCategory: (category: Category) => void
  readonly hidden: boolean
  readonly initialProjectName: string | null
  readonly assetListEvents: assetListEvent.AssetListEvent[]
  readonly dispatchAssetListEvent: (directoryEvent: assetListEvent.AssetListEvent) => void
  readonly assetEvents: assetEvent.AssetEvent[]
  readonly dispatchAssetEvent: (directoryEvent: assetEvent.AssetEvent) => void
  readonly labels: backendModule.Label[]
  readonly setLabels: React.Dispatch<React.SetStateAction<backendModule.Label[]>>
  readonly projectStartupInfo: backendModule.ProjectStartupInfo | null
  readonly doOpenEditor: (
    backend: Backend,
    project: backendModule.ProjectAsset,
    setProject: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>,
    switchPage: boolean
  ) => void
  readonly doCloseEditor: (project: backendModule.ProjectAsset) => void
}

/** Contains directory path and directory contents (projects, folders, secrets and files). */
export default function Drive(props: DriveProps) {
  const { hidden, initialProjectName, labels, setLabels, projectStartupInfo } = props
  const { assetListEvents, dispatchAssetListEvent, assetEvents, dispatchAssetEvent } = props
  const { doOpenEditor, doCloseEditor, category, setCategory } = props

  const navigate = navigateHooks.useNavigate()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { type: sessionType, user } = authProvider.useNonPartialUserSession()
  const remoteBackend = backendProvider.useRemoteBackend()
  const localBackend = backendProvider.useLocalBackend()
  const backend = backendProvider.useBackend(category)
  const { localStorage } = localStorageProvider.useLocalStorage()
  const { getText } = textProvider.useText()
  const [query, setQuery] = React.useState(() => AssetQuery.fromString(''))
  const [suggestions, setSuggestions] = React.useState<assetSearchBar.Suggestion[]>([])
  const [canDownload, setCanDownload] = React.useState(false)
  const [didLoadingProjectManagerFail, setDidLoadingProjectManagerFail] = React.useState(false)
  const [newLabelNames, setNewLabelNames] = React.useState(new Set<backendModule.LabelName>())
  const [assetPanelProps, setAssetPanelProps] =
    React.useState<assetPanel.AssetPanelRequiredProps | null>(null)
  const [isAssetPanelEnabled, setIsAssetPanelEnabled] = React.useState(
    () => localStorage.get('isAssetPanelVisible') ?? false
  )
  const [isAssetPanelTemporarilyVisible, setIsAssetPanelTemporarilyVisible] = React.useState(false)
  const [deletedLabelNames, setDeletedLabelNames] = React.useState(
    new Set<backendModule.LabelName>()
  )
  const allLabels = React.useMemo(
    () => new Map(labels.map(label => [label.value, label])),
    [labels]
  )
  const rootDirectoryId = React.useMemo(
    () => backend.rootDirectoryId(user) ?? backendModule.DirectoryId(''),
    [backend, user]
  )
  const targetDirectoryNodeRef = React.useRef<AssetTreeNode<backendModule.DirectoryAsset> | null>(
    null
  )
  const isCloud = categoryModule.isCloud(category)
  const status =
    !isCloud && didLoadingProjectManagerFail
      ? DriveStatus.noProjectManager
      : isCloud && sessionType === authProvider.UserSessionType.offline
        ? DriveStatus.offline
        : isCloud && user?.isEnabled !== true
          ? DriveStatus.notEnabled
          : DriveStatus.ok
  const isAssetPanelVisible = isAssetPanelEnabled || isAssetPanelTemporarilyVisible

  const onSetCategory = eventCallback.useEventCallback((value: Category) => {
    setCategory(value)
    localStorage.set('driveCategory', value)
  })

  React.useEffect(() => {
    localStorage.set('isAssetPanelVisible', isAssetPanelEnabled)
  }, [isAssetPanelEnabled, /* should never change */ localStorage])

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
      if (remoteBackend != null && user?.isEnabled === true) {
        setLabels(await remoteBackend.listTags())
      }
    })()
  }, [remoteBackend, user?.isEnabled, /* should never change */ setLabels])

  const doUploadFiles = React.useCallback(
    (files: File[]) => {
      if (isCloud && sessionType === authProvider.UserSessionType.offline) {
        // This should never happen, however display a nice error message in case it does.
        toastAndLog('offlineUploadFilesError')
      } else {
        dispatchAssetListEvent({
          type: AssetListEventType.uploadFiles,
          parentKey: targetDirectoryNodeRef.current?.key ?? rootDirectoryId,
          parentId: targetDirectoryNodeRef.current?.item.id ?? rootDirectoryId,
          files,
        })
      }
    },
    [
      isCloud,
      rootDirectoryId,
      sessionType,
      toastAndLog,
      /* should never change */ dispatchAssetListEvent,
    ]
  )

  const doEmptyTrash = React.useCallback(() => {
    dispatchAssetListEvent({ type: AssetListEventType.emptyTrash })
  }, [/* should never change */ dispatchAssetListEvent])

  const doCreateProject = React.useCallback(
    (templateId: string | null = null, templateName: string | null = null) => {
      dispatchAssetListEvent({
        type: AssetListEventType.newProject,
        parentKey: targetDirectoryNodeRef.current?.key ?? rootDirectoryId,
        parentId: targetDirectoryNodeRef.current?.item.id ?? rootDirectoryId,
        templateId,
        datalinkId: null,
        preferredName: templateName,
      })
    },
    [rootDirectoryId, /* should never change */ dispatchAssetListEvent]
  )

  const doCreateDirectory = React.useCallback(() => {
    dispatchAssetListEvent({
      type: AssetListEventType.newFolder,
      parentKey: targetDirectoryNodeRef.current?.key ?? rootDirectoryId,
      parentId: targetDirectoryNodeRef.current?.item.id ?? rootDirectoryId,
    })
  }, [rootDirectoryId, /* should never change */ dispatchAssetListEvent])

  const doCreateLabel = React.useCallback(
    async (value: string, color: backendModule.LChColor) => {
      if (remoteBackend == null) {
        // eslint-disable-next-line no-restricted-syntax
        throw new Error('Labels can only be created on the Remote Backend.')
      } else {
        const newLabelName = backendModule.LabelName(value)
        const placeholderLabel: backendModule.Label = {
          id: backendModule.TagId(uniqueString.uniqueString()),
          value: newLabelName,
          color,
        }
        setNewLabelNames(labelNames => new Set([...labelNames, newLabelName]))
        setLabels(oldLabels => [...oldLabels, placeholderLabel])
        try {
          const newLabel = await remoteBackend.createTag({ value, color })
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
      }
    },
    [remoteBackend, toastAndLog, /* should never change */ setLabels]
  )

  const doDeleteLabel = React.useCallback(
    async (id: backendModule.TagId, value: backendModule.LabelName) => {
      if (remoteBackend == null) {
        // eslint-disable-next-line no-restricted-syntax
        throw new Error('Labels can only be deleted on the Remote Backend.')
      } else {
        setDeletedLabelNames(oldNames => new Set([...oldNames, value]))
        setQuery(oldQuery => oldQuery.deleteFromEveryTerm({ labels: [value] }))
        try {
          await remoteBackend.deleteTag(id, value)
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
      }
    },
    [
      remoteBackend,
      toastAndLog,
      /* should never change */ setQuery,
      /* should never change */ dispatchAssetEvent,
      /* should never change */ setLabels,
    ]
  )

  const doCreateSecret = React.useCallback(
    (name: string, value: string) => {
      dispatchAssetListEvent({
        type: AssetListEventType.newSecret,
        parentKey: targetDirectoryNodeRef.current?.key ?? rootDirectoryId,
        parentId: targetDirectoryNodeRef.current?.item.id ?? rootDirectoryId,
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
        parentKey: targetDirectoryNodeRef.current?.key ?? rootDirectoryId,
        parentId: targetDirectoryNodeRef.current?.item.id ?? rootDirectoryId,
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
        <result.Result
          status="error"
          title={getText('notEnabledTitle')}
          qa="not-enabled-stub"
          subtitle={`${getText('notEnabledSubtitle')}${localBackend == null ? ' ' + getText('downloadFreeEditionMessage') : ''}`}
        >
          {localBackend == null && (
            <ariaComponents.Button
              variant="primary"
              size="medium"
              rounding="full"
              data-testid="download-free-edition"
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
            </ariaComponents.Button>
          )}
        </result.Result>
      )
    }
    case DriveStatus.ok: {
      return (
        <div className={`relative flex grow ${hidden ? 'hidden' : ''}`}>
          <div
            data-testid="drive-view"
            className="mt-4 flex flex-1 flex-col gap-4 overflow-hidden px-page-x"
          >
            <DriveBar
              query={query}
              setQuery={setQuery}
              labels={labels}
              suggestions={suggestions}
              category={category}
              canDownload={canDownload}
              isAssetPanelOpen={isAssetPanelVisible}
              setIsAssetPanelOpen={valueOrUpdater => {
                const newValue =
                  typeof valueOrUpdater === 'function'
                    ? valueOrUpdater(isAssetPanelVisible)
                    : valueOrUpdater
                setIsAssetPanelTemporarilyVisible(false)
                setIsAssetPanelEnabled(newValue)
              }}
              doEmptyTrash={doEmptyTrash}
              doCreateProject={doCreateProject}
              doUploadFiles={doUploadFiles}
              doCreateDirectory={doCreateDirectory}
              doCreateSecret={doCreateSecret}
              doCreateDataLink={doCreateDataLink}
              dispatchAssetEvent={dispatchAssetEvent}
            />
            <div className="flex flex-1 gap-drive overflow-hidden">
              <div className="flex w-drive-sidebar flex-col gap-drive-sidebar py-drive-sidebar-y">
                <CategorySwitcher
                  category={category}
                  setCategory={onSetCategory}
                  dispatchAssetEvent={dispatchAssetEvent}
                />
                {isCloud && (
                  <Labels
                    draggable={category !== Category.trash}
                    labels={labels}
                    query={query}
                    setQuery={setQuery}
                    doCreateLabel={doCreateLabel}
                    doDeleteLabel={doDeleteLabel}
                    newLabelNames={newLabelNames}
                    deletedLabelNames={deletedLabelNames}
                  />
                )}
              </div>
              <AssetsTable
                hidden={hidden}
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
            </div>
          </div>
          <div
            className={`flex flex-col overflow-hidden transition-min-width duration-side-panel ease-in-out ${
              isAssetPanelVisible ? 'min-w-side-panel' : 'invisible min-w'
            }`}
          >
            {isAssetPanelVisible && (
              <AssetPanel
                key={assetPanelProps?.item?.item.id}
                backend={assetPanelProps?.backend ?? null}
                item={assetPanelProps?.item ?? null}
                setItem={assetPanelProps?.setItem ?? null}
                category={category}
                labels={labels}
                dispatchAssetEvent={dispatchAssetEvent}
                isReadonly={category === Category.trash}
              />
            )}
          </div>
        </div>
      )
    }
  }
}
