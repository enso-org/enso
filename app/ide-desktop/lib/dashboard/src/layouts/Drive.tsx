/** @file The directory header bar and directory item listing. */
import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import * as appUtils from '#/appUtils'

import * as eventCallback from '#/hooks/eventCallbackHooks'
import * as navigateHooks from '#/hooks/navigateHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as assetEvent from '#/events/assetEvent'
import type * as assetListEvent from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'

import type * as assetPanel from '#/layouts/AssetPanel'
import type * as assetSearchBar from '#/layouts/AssetSearchBar'
import AssetsTable from '#/layouts/AssetsTable'
import CategorySwitcher from '#/layouts/CategorySwitcher'
import Category, * as categoryModule from '#/layouts/CategorySwitcher/Category'
import DriveBar from '#/layouts/DriveBar'
import Labels from '#/layouts/Labels'

import * as ariaComponents from '#/components/AriaComponents'
import * as result from '#/components/Result'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'
import * as projectManager from '#/services/ProjectManager'

import type AssetQuery from '#/utilities/AssetQuery'
import type AssetTreeNode from '#/utilities/AssetTreeNode'
import * as download from '#/utilities/download'
import * as github from '#/utilities/github'

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
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly setSuggestions: (suggestions: assetSearchBar.Suggestion[]) => void
  readonly projectStartupInfo: backendModule.ProjectStartupInfo | null
  readonly setProjectStartupInfo: (projectStartupInfo: backendModule.ProjectStartupInfo) => void
  readonly setAssetPanelProps: (props: assetPanel.AssetPanelRequiredProps | null) => void
  readonly setIsAssetPanelTemporarilyVisible: (visible: boolean) => void
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
  const { hidden, initialProjectName, query, setQuery } = props
  const { setSuggestions, projectStartupInfo, setProjectStartupInfo } = props
  const { assetListEvents, dispatchAssetListEvent, assetEvents, dispatchAssetEvent } = props
  const { setAssetPanelProps, doOpenEditor, doCloseEditor } = props
  const { setIsAssetPanelTemporarilyVisible, category, setCategory } = props

  const navigate = navigateHooks.useNavigate()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { type: sessionType, user } = authProvider.useNonPartialUserSession()
  const localBackend = backendProvider.useLocalBackend()
  const backend = backendProvider.useBackend(category)
  const { localStorage } = localStorageProvider.useLocalStorage()
  const { getText } = textProvider.useText()
  const [canDownload, setCanDownload] = React.useState(false)
  const [didLoadingProjectManagerFail, setDidLoadingProjectManagerFail] = React.useState(false)
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

  const doCreateDatalink = React.useCallback(
    (name: string, value: unknown) => {
      dispatchAssetListEvent({
        type: AssetListEventType.newDatalink,
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
        <div className={tailwindMerge.twMerge('grid grow place-items-center', hidden && 'hidden')}>
          <div className="flex flex-col gap-status-page text-center text-base">
            <div>{getText('youAreNotLoggedIn')}</div>
            <ariaComponents.Button
              size="custom"
              variant="custom"
              className="button self-center bg-help text-white"
              onPress={() => {
                navigate(appUtils.LOGIN_PATH)
              }}
            >
              {getText('login')}
            </ariaComponents.Button>
          </div>
        </div>
      )
    }
    case DriveStatus.noProjectManager: {
      return (
        <div className={tailwindMerge.twMerge('grid grow place-items-center', hidden && 'hidden')}>
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
          testId="not-enabled-stub"
          subtitle={`${getText('notEnabledSubtitle')}${localBackend == null ? ' ' + getText('downloadFreeEditionMessage') : ''}`}
        >
          <ariaComponents.ButtonGroup align="center">
            <ariaComponents.Button variant="tertiary" size="medium" href={appUtils.SUBSCRIBE_PATH}>
              {getText('upgrade')}
            </ariaComponents.Button>
            {localBackend == null && (
              <ariaComponents.Button
                variant="primary"
                size="medium"
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
          </ariaComponents.ButtonGroup>
        </result.Result>
      )
    }
    case DriveStatus.ok: {
      return (
        <div
          data-testid="drive-view"
          className={tailwindMerge.twMerge(
            'flex flex-1 flex-col gap-drive-heading overflow-visible px-page-x',
            hidden && 'hidden'
          )}
        >
          <DriveBar
            category={category}
            canDownload={canDownload}
            doEmptyTrash={doEmptyTrash}
            doCreateProject={doCreateProject}
            doUploadFiles={doUploadFiles}
            doCreateDirectory={doCreateDirectory}
            doCreateSecret={doCreateSecret}
            doCreateDatalink={doCreateDatalink}
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
                  backend={backend}
                  draggable={category !== Category.trash}
                  query={query}
                  setQuery={setQuery}
                />
              )}
            </div>
            <AssetsTable
              hidden={hidden}
              query={query}
              setQuery={setQuery}
              setCanDownload={setCanDownload}
              setProjectStartupInfo={setProjectStartupInfo}
              category={category}
              setSuggestions={setSuggestions}
              initialProjectName={initialProjectName}
              projectStartupInfo={projectStartupInfo}
              assetEvents={assetEvents}
              dispatchAssetEvent={dispatchAssetEvent}
              assetListEvents={assetListEvents}
              dispatchAssetListEvent={dispatchAssetListEvent}
              setAssetPanelProps={setAssetPanelProps}
              setIsAssetPanelTemporarilyVisible={setIsAssetPanelTemporarilyVisible}
              targetDirectoryNodeRef={targetDirectoryNodeRef}
              doOpenEditor={doOpenEditor}
              doCloseEditor={doCloseEditor}
            />
          </div>
        </div>
      )
    }
  }
}
