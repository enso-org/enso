/** @file An interactive button indicating the status of a project. */
import * as React from 'react'

import * as toast from 'react-toastify'

import ArrowUpIcon from 'enso-assets/arrow_up.svg'
import LogsIcon from 'enso-assets/logs.svg'
import PlayIcon from 'enso-assets/play.svg'
import StopIcon from 'enso-assets/stop.svg'
import TriangleDownIcon from 'enso-assets/triangle_down.svg'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'
import * as eventHooks from '#/hooks/eventHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'
import type * as assetsTable from '#/layouts/dashboard/AssetsTable'
import LogsModal from '#/layouts/dashboard/LogsModal'
import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as backendModule from '#/services/backend'
import * as remoteBackend from '#/services/remoteBackend'
import type * as assetTreeNode from '#/utilities/assetTreeNode'
import * as errorModule from '#/utilities/error'
import * as localStorageModule from '#/utilities/localStorage'
import * as object from '#/utilities/object'

import Spinner, * as spinner from '#/components/Spinner'
import SvgMask from '#/components/SvgMask'

// =================
// === Constants ===
// =================

/** The size of the icon, in pixels. */
const ICON_SIZE_PX = 24
/** The styles of the icons. */
const ICON_CLASSES = 'w-6 h-6'
const LOADING_MESSAGE =
  'Your environment is being created. It will take some time, please be patient.'
/** The corresponding {@link spinner.SpinnerState} for each {@link backendModule.ProjectState},
 * when using the remote backend. */
const REMOTE_SPINNER_STATE: Record<backendModule.ProjectState, spinner.SpinnerState> = {
  [backendModule.ProjectState.closed]: spinner.SpinnerState.initial,
  [backendModule.ProjectState.closing]: spinner.SpinnerState.initial,
  [backendModule.ProjectState.created]: spinner.SpinnerState.initial,
  [backendModule.ProjectState.new]: spinner.SpinnerState.initial,
  [backendModule.ProjectState.placeholder]: spinner.SpinnerState.loadingSlow,
  [backendModule.ProjectState.openInProgress]: spinner.SpinnerState.loadingSlow,
  [backendModule.ProjectState.provisioned]: spinner.SpinnerState.loadingSlow,
  [backendModule.ProjectState.cloning]: spinner.SpinnerState.loadingSlow,
  [backendModule.ProjectState.opened]: spinner.SpinnerState.done,
}
/** The corresponding {@link spinner.SpinnerState} for each {@link backendModule.ProjectState},
 * when using the local backend. */
const LOCAL_SPINNER_STATE: Record<backendModule.ProjectState, spinner.SpinnerState> = {
  [backendModule.ProjectState.closed]: spinner.SpinnerState.initial,
  [backendModule.ProjectState.closing]: spinner.SpinnerState.initial,
  [backendModule.ProjectState.created]: spinner.SpinnerState.initial,
  [backendModule.ProjectState.new]: spinner.SpinnerState.initial,
  [backendModule.ProjectState.placeholder]: spinner.SpinnerState.loadingMedium,
  [backendModule.ProjectState.openInProgress]: spinner.SpinnerState.loadingMedium,
  [backendModule.ProjectState.provisioned]: spinner.SpinnerState.loadingMedium,
  [backendModule.ProjectState.cloning]: spinner.SpinnerState.loadingMedium,
  [backendModule.ProjectState.opened]: spinner.SpinnerState.done,
}

// ===================
// === ProjectIcon ===
// ===================

/** Props for a {@link ProjectIcon}. */
export interface ProjectIconProps {
  keyProp: string
  item: assetTreeNode.AssetTreeNode
  setItem: React.Dispatch<React.SetStateAction<assetTreeNode.AssetTreeNode>>
  asset: backendModule.ProjectAsset
  setAsset: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>
  isHovered: boolean
  setRowState: React.Dispatch<React.SetStateAction<assetsTable.AssetRowState>>
  assetEvents: assetEvent.AssetEvent[]
  /** Called when the project is opened via the {@link ProjectIcon}. */
  doOpenManually: (projectId: backendModule.ProjectId) => void
  onClose: () => void
  openIde: (switchPage: boolean) => void
}

/** An interactive icon indicating the status of a project. */
export default function ProjectIcon(props: ProjectIconProps) {
  const {
    keyProp: key,
    item,
    setItem,
    asset,
    setAsset,
    isHovered,
    assetEvents,
    doOpenManually,
    onClose,
    openIde,
  } = props
  const { backend } = backendProvider.useBackend()
  const { organization } = authProvider.useNonPartialUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [shouldAnimate, setShouldAnimate] = React.useState(false)
  const state = asset.projectState.type
  const setState = React.useCallback(
    (stateOrUpdater: React.SetStateAction<backendModule.ProjectState>) => {
      setAsset(oldItem => {
        let newState: backendModule.ProjectState
        if (typeof stateOrUpdater === 'function') {
          newState = stateOrUpdater(oldItem.projectState.type)
        } else {
          newState = stateOrUpdater
        }
        let newProjectState: backendModule.ProjectStateType = object.merge(oldItem.projectState, {
          type: newState,
        })
        if (!backendModule.DOES_PROJECT_STATE_INDICATE_VM_EXISTS[newState]) {
          // eslint-disable-next-line @typescript-eslint/naming-convention, @typescript-eslint/no-unused-vars
          const { opened_by, ...newProjectState2 } = newProjectState
          newProjectState = newProjectState2
        } else if (organization != null) {
          newProjectState = object.merge(newProjectState, {
            // eslint-disable-next-line @typescript-eslint/naming-convention
            opened_by: organization.email,
          })
        }
        return object.merge(oldItem, { projectState: newProjectState })
      })
    },
    [organization, /* should never change */ setAsset]
  )
  const [spinnerState, setSpinnerState] = React.useState(spinner.SpinnerState.initial)
  const [onSpinnerStateChange, setOnSpinnerStateChange] = React.useState<
    ((state: spinner.SpinnerState | null) => void) | null
  >(null)
  const [shouldOpenWhenReady, setShouldOpenWhenReady] = React.useState(false)
  const [isRunningInBackground, setIsRunningInBackground] = React.useState(
    asset.projectState.execute_async ?? false
  )
  const [shouldSwitchPage, setShouldSwitchPage] = React.useState(false)
  const [toastId, setToastId] = React.useState<toast.Id | null>(null)
  const [openProjectAbortController, setOpenProjectAbortController] =
    React.useState<AbortController | null>(null)
  const [closeProjectAbortController, setCloseProjectAbortController] =
    React.useState<AbortController | null>(null)
  const isOtherUserUsingProject =
    backend.type !== backendModule.BackendType.local &&
    asset.projectState.opened_by !== organization?.email

  React.useEffect(() => {
    if (isHovered) {
      // Delay adding animation CSS attributes, to prevent animations for
      // the initial hover.
      requestAnimationFrame(() => {
        setShouldAnimate(true)
      })
    } else {
      setShouldAnimate(false)
    }
  }, [isHovered])

  const openProject = React.useCallback(
    async (shouldRunInBackground: boolean) => {
      closeProjectAbortController?.abort()
      setCloseProjectAbortController(null)
      setState(backendModule.ProjectState.openInProgress)
      try {
        switch (backend.type) {
          case backendModule.BackendType.remote: {
            if (state !== backendModule.ProjectState.opened) {
              if (!shouldRunInBackground) {
                setToastId(toast.toast.loading(LOADING_MESSAGE))
              }
              await backend.openProject(
                asset.id,
                {
                  forceCreate: false,
                  executeAsync: shouldRunInBackground,
                },
                asset.title
              )
            }
            const abortController = new AbortController()
            setOpenProjectAbortController(abortController)
            await remoteBackend.waitUntilProjectIsReady(backend, asset, abortController)
            setToastId(null)
            if (!abortController.signal.aborted) {
              setState(oldState =>
                oldState === backendModule.ProjectState.openInProgress
                  ? backendModule.ProjectState.opened
                  : oldState
              )
            }
            break
          }
          case backendModule.BackendType.local: {
            await backend.openProject(
              asset.id,
              {
                forceCreate: false,
                executeAsync: shouldRunInBackground,
              },
              asset.title
            )
            setState(oldState =>
              oldState === backendModule.ProjectState.openInProgress
                ? backendModule.ProjectState.opened
                : oldState
            )
            break
          }
        }
      } catch (error) {
        const project = await backend.getProjectDetails(asset.id, asset.title)
        setAsset(object.merger({ projectState: project.state }))
        toastAndLog(
          errorModule.tryGetMessage(error)?.slice(0, -1) ??
            `Could not open project '${asset.title}'`
        )
        setState(backendModule.ProjectState.closed)
      }
    },
    [
      state,
      backend,
      asset,
      closeProjectAbortController,
      /* should never change */ toastAndLog,
      /* should never change */ setState,
      /* should never change */ setAsset,
    ]
  )

  React.useEffect(() => {
    if (toastId != null) {
      return () => {
        toast.toast.dismiss(toastId)
      }
    } else {
      return
    }
  }, [toastId])

  React.useEffect(() => {
    // Ensure that the previous spinner state is visible for at least one frame.
    requestAnimationFrame(() => {
      const newSpinnerState =
        backend.type === backendModule.BackendType.remote
          ? REMOTE_SPINNER_STATE[state]
          : LOCAL_SPINNER_STATE[state]
      setSpinnerState(newSpinnerState)
      onSpinnerStateChange?.(state === backendModule.ProjectState.closed ? null : newSpinnerState)
    })
  }, [state, backend.type, onSpinnerStateChange])

  React.useEffect(() => {
    onSpinnerStateChange?.(spinner.SpinnerState.initial)
    return () => {
      onSpinnerStateChange?.(null)
    }
  }, [onSpinnerStateChange])

  eventHooks.useEventHandler(assetEvents, event => {
    switch (event.type) {
      case AssetEventType.newFolder:
      case AssetEventType.uploadFiles:
      case AssetEventType.newDataConnector:
      case AssetEventType.copy:
      case AssetEventType.cut:
      case AssetEventType.cancelCut:
      case AssetEventType.move:
      case AssetEventType.delete:
      case AssetEventType.restore:
      case AssetEventType.download:
      case AssetEventType.downloadSelected:
      case AssetEventType.removeSelf:
      case AssetEventType.temporarilyAddLabels:
      case AssetEventType.temporarilyRemoveLabels:
      case AssetEventType.addLabels:
      case AssetEventType.removeLabels:
      case AssetEventType.deleteLabel: {
        // Ignored. Any missing project-related events should be handled by
        // `ProjectNameColumn`. `deleteMultiple`, `restoreMultiple`, `download`,
        // and `downloadSelected` are handled by `AssetRow`.
        break
      }
      case AssetEventType.openProject: {
        if (event.id !== asset.id) {
          if (!event.runInBackground && !isRunningInBackground) {
            setShouldOpenWhenReady(false)
            if (!isOtherUserUsingProject) {
              void closeProject(false)
            }
          }
        } else {
          setShouldOpenWhenReady(!event.runInBackground)
          setShouldSwitchPage(event.shouldAutomaticallySwitchPage)
          setIsRunningInBackground(event.runInBackground)
          void openProject(event.runInBackground)
        }
        break
      }
      case AssetEventType.closeProject: {
        if (event.id === asset.id) {
          setShouldOpenWhenReady(false)
          void closeProject(false)
        }
        break
      }
      case AssetEventType.cancelOpeningAllProjects: {
        if (!isRunningInBackground) {
          setShouldOpenWhenReady(false)
          onSpinnerStateChange?.(null)
          setOnSpinnerStateChange(null)
          openProjectAbortController?.abort()
          setOpenProjectAbortController(null)
          if (!isOtherUserUsingProject) {
            void closeProject(false)
          }
        }
        break
      }
      case AssetEventType.newProject: {
        if (event.placeholderId === key) {
          setOnSpinnerStateChange(() => event.onSpinnerStateChange)
        } else if (event.onSpinnerStateChange === onSpinnerStateChange) {
          setOnSpinnerStateChange(null)
        }
        break
      }
    }
  })

  React.useEffect(() => {
    if (state === backendModule.ProjectState.opened) {
      if (shouldOpenWhenReady) {
        openIde(shouldSwitchPage)
        setShouldOpenWhenReady(false)
      }
    }
    // `openIde` is a callback, not a dependency.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [shouldOpenWhenReady, shouldSwitchPage, state])

  const closeProject = async (triggerOnClose = true) => {
    if (triggerOnClose) {
      onClose()
      localStorage.delete(localStorageModule.LocalStorageKey.projectStartupInfo)
    }
    setToastId(null)
    setShouldOpenWhenReady(false)
    setState(backendModule.ProjectState.closing)
    onSpinnerStateChange?.(null)
    setOnSpinnerStateChange(null)
    openProjectAbortController?.abort()
    setOpenProjectAbortController(null)
    const abortController = new AbortController()
    setCloseProjectAbortController(abortController)
    if (backendModule.DOES_PROJECT_STATE_INDICATE_VM_EXISTS[state]) {
      try {
        if (
          backend.type === backendModule.BackendType.local &&
          state === backendModule.ProjectState.openInProgress
        ) {
          // Projects that are not opened cannot be closed.
          // This is the only way to wait until the project is open.
          await backend.openProject(asset.id, null, asset.title)
        }
        try {
          await backend.closeProject(asset.id, asset.title)
        } catch {
          // Ignored. The project is already closed.
        }
      } finally {
        if (!abortController.signal.aborted) {
          setState(backendModule.ProjectState.closed)
        }
      }
    }
  }

  const triangleButton = (
    <SvgMask
      src={TriangleDownIcon}
      className={`cursor-pointer h-4 w-4 m-1 ${
        shouldAnimate ? 'transition-transform duration-300' : ''
      } ${item.isProjectExpanded && item.children != null ? '' : '-rotate-90'}`}
      onClick={event => {
        event.stopPropagation()
        setItem(oldItem => oldItem.with({ isProjectExpanded: !oldItem.isProjectExpanded }))
      }}
    />
  )

  switch (state) {
    case null:
    case backendModule.ProjectState.created:
    case backendModule.ProjectState.new:
    case backendModule.ProjectState.closing:
    case backendModule.ProjectState.closed:
      return isHovered && item.children != null ? (
        triangleButton
      ) : (
        <button
          className="w-6 h-6 disabled:opacity-50"
          onClick={clickEvent => {
            clickEvent.stopPropagation()
            unsetModal()
            doOpenManually(asset.id)
          }}
        >
          <SvgMask alt="Open in editor" className={ICON_CLASSES} src={PlayIcon} />
        </button>
      )
    case backendModule.ProjectState.openInProgress:
    case backendModule.ProjectState.provisioned:
    case backendModule.ProjectState.cloning:
    case backendModule.ProjectState.placeholder:
      return isHovered && item.children != null ? (
        triangleButton
      ) : (
        <button
          disabled={isOtherUserUsingProject}
          {...(isOtherUserUsingProject ? { title: 'Someone else is using this project.' } : {})}
          className="w-6 h-6 disabled:opacity-50"
          onClick={async clickEvent => {
            clickEvent.stopPropagation()
            unsetModal()
            await closeProject(!isRunningInBackground)
          }}
        >
          <div className={`relative h-0 ${isRunningInBackground ? 'text-green' : ''}`}>
            <Spinner size={ICON_SIZE_PX} state={spinnerState} />
          </div>
          <SvgMask
            alt="Stop execution"
            src={StopIcon}
            className={`${ICON_CLASSES} ${isRunningInBackground ? 'text-green' : ''}`}
          />
        </button>
      )
    case backendModule.ProjectState.opened:
      return isHovered && item.children != null ? (
        triangleButton
      ) : (
        <div>
          <button
            disabled={isOtherUserUsingProject}
            {...(isOtherUserUsingProject ? { title: 'Someone else has this project open.' } : {})}
            className="w-6 h-6 disabled:opacity-50"
            onClick={async clickEvent => {
              clickEvent.stopPropagation()
              unsetModal()
              await closeProject(!isRunningInBackground)
            }}
          >
            <div className={`relative h-0 ${isRunningInBackground ? 'text-green' : ''}`}>
              <Spinner size={24} state={spinnerState} />
            </div>
            <SvgMask
              alt="Stop execution"
              src={StopIcon}
              className={`${ICON_CLASSES} ${isRunningInBackground ? 'text-green' : ''}`}
            />
          </button>
          {!isOtherUserUsingProject && !isRunningInBackground && (
            <button
              className="w-6 h-6"
              onClick={clickEvent => {
                clickEvent.stopPropagation()
                unsetModal()
                openIde(true)
              }}
            >
              <SvgMask alt="Open in editor" src={ArrowUpIcon} className={ICON_CLASSES} />
            </button>
          )}
          {isRunningInBackground && (
            <button
              className="w-6 h-6"
              onClick={async clickEvent => {
                clickEvent.stopPropagation()
                unsetModal()
                const logs = await backend.getLogs(asset.id, asset.title)
                setModal(<LogsModal logs={logs} />)
              }}
            >
              <SvgMask alt="Show logs" src={LogsIcon} className={ICON_CLASSES} />
            </button>
          )}
        </div>
      )
  }
}
