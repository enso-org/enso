/** @file An interactive button indicating the status of a project. */
import * as React from 'react'

import * as toast from 'react-toastify'

import ArrowUpIcon from 'enso-assets/arrow_up.svg'
import PlayIcon from 'enso-assets/play.svg'
import StopIcon from 'enso-assets/stop.svg'

import * as eventHooks from '#/hooks/eventHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as sessionProvider from '#/providers/SessionProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'

import type * as assetsTable from '#/layouts/AssetsTable'

import Spinner, * as spinner from '#/components/Spinner'
import SvgMask from '#/components/SvgMask'
import UnstyledButton from '#/components/UnstyledButton'

import * as backendModule from '#/services/Backend'

import * as object from '#/utilities/object'

// =================
// === Constants ===
// =================

/** The size of the icon, in pixels. */
const ICON_SIZE_PX = 24
const LOADING_MESSAGE =
  'Your environment is being created. It will take some time, please be patient.'
/** The corresponding {@link spinner.SpinnerState} for each {@link backendModule.ProjectState},
 * when using the remote backend. */
const REMOTE_SPINNER_STATE: Readonly<Record<backendModule.ProjectState, spinner.SpinnerState>> = {
  [backendModule.ProjectState.closed]: spinner.SpinnerState.initial,
  [backendModule.ProjectState.closing]: spinner.SpinnerState.initial,
  [backendModule.ProjectState.created]: spinner.SpinnerState.initial,
  [backendModule.ProjectState.new]: spinner.SpinnerState.initial,
  [backendModule.ProjectState.placeholder]: spinner.SpinnerState.loadingSlow,
  [backendModule.ProjectState.openInProgress]: spinner.SpinnerState.loadingSlow,
  [backendModule.ProjectState.provisioned]: spinner.SpinnerState.loadingSlow,
  [backendModule.ProjectState.scheduled]: spinner.SpinnerState.loadingSlow,
  [backendModule.ProjectState.opened]: spinner.SpinnerState.done,
}
/** The corresponding {@link spinner.SpinnerState} for each {@link backendModule.ProjectState},
 * when using the local backend. */
const LOCAL_SPINNER_STATE: Readonly<Record<backendModule.ProjectState, spinner.SpinnerState>> = {
  [backendModule.ProjectState.closed]: spinner.SpinnerState.initial,
  [backendModule.ProjectState.closing]: spinner.SpinnerState.initial,
  [backendModule.ProjectState.created]: spinner.SpinnerState.initial,
  [backendModule.ProjectState.new]: spinner.SpinnerState.initial,
  [backendModule.ProjectState.placeholder]: spinner.SpinnerState.loadingMedium,
  [backendModule.ProjectState.openInProgress]: spinner.SpinnerState.loadingMedium,
  [backendModule.ProjectState.provisioned]: spinner.SpinnerState.loadingMedium,
  [backendModule.ProjectState.scheduled]: spinner.SpinnerState.loadingMedium,
  [backendModule.ProjectState.opened]: spinner.SpinnerState.done,
}

// ===================
// === ProjectIcon ===
// ===================

/** Props for a {@link ProjectIcon}. */
export interface ProjectIconProps {
  readonly smartAsset: backendModule.SmartProject
  readonly setItem: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>
  readonly assetEvents: assetEvent.AssetEvent[]
  readonly state: assetsTable.AssetsTableState
  /** Called when the project is opened via the {@link ProjectIcon}. */
  readonly doOpenManually: (projectId: backendModule.ProjectId) => void
  readonly doOpenEditor: (switchPage: boolean) => void
  readonly doCloseEditor: () => void
}

/** An interactive icon indicating the status of a project. */
export default function ProjectIcon(props: ProjectIconProps) {
  const { smartAsset, setItem, state, assetEvents, doOpenManually } = props
  const { doCloseEditor, doOpenEditor } = props
  const { isCloud } = state
  const { session } = sessionProvider.useSession()
  const { user } = authProvider.useNonPartialUserSession()
  const { unsetModal } = modalProvider.useSetModal()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { getText } = textProvider.useText()
  const asset = smartAsset.value
  const projectState = asset.projectState.type
  const setProjectState = React.useCallback(
    (stateOrUpdater: React.SetStateAction<backendModule.ProjectState>) => {
      setItem(oldItem => {
        let newState: backendModule.ProjectState
        if (typeof stateOrUpdater === 'function') {
          newState = stateOrUpdater(oldItem.projectState.type)
        } else {
          newState = stateOrUpdater
        }
        let newProjectState: backendModule.ProjectStateType = object.merge(oldItem.projectState, {
          type: newState,
        })
        if (!backendModule.IS_OPENING_OR_OPENED[newState]) {
          // eslint-disable-next-line @typescript-eslint/naming-convention, @typescript-eslint/no-unused-vars
          const { opened_by, ...newProjectState2 } = newProjectState
          newProjectState = newProjectState2
        } else if (user != null) {
          // eslint-disable-next-line @typescript-eslint/naming-convention
          newProjectState = object.merge(newProjectState, { opened_by: user.value.email })
        }
        return object.merge(oldItem, { projectState: newProjectState })
      })
    },
    [user, /* should never change */ setItem]
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
  const isOtherUserUsingProject = isCloud && asset.projectState.opened_by !== user?.value.email

  const openProject = React.useCallback(
    async (shouldRunInBackground: boolean) => {
      closeProjectAbortController?.abort()
      setCloseProjectAbortController(null)
      setProjectState(backendModule.ProjectState.openInProgress)
      try {
        if (isCloud) {
          if (projectState !== backendModule.ProjectState.opened) {
            if (!shouldRunInBackground) {
              setToastId(toast.toast.loading(LOADING_MESSAGE))
            }
            await smartAsset.open({
              executeAsync: shouldRunInBackground,
              parentId: smartAsset.value.parentId,
              cognitoCredentials: session,
            })
          }
          const abortController = new AbortController()
          setOpenProjectAbortController(abortController)
          await smartAsset.waitUntilReady(abortController)
          setToastId(null)
          if (!abortController.signal.aborted) {
            setProjectState(oldState =>
              oldState === backendModule.ProjectState.openInProgress
                ? backendModule.ProjectState.opened
                : oldState
            )
          }
        } else {
          await smartAsset.open({
            executeAsync: shouldRunInBackground,
            parentId: smartAsset.value.parentId,
            cognitoCredentials: session,
          })
          setProjectState(oldState =>
            oldState === backendModule.ProjectState.openInProgress
              ? backendModule.ProjectState.opened
              : oldState
          )
        }
      } catch (error) {
        const project = await smartAsset.getDetails()
        setItem(object.merger({ projectState: project.state }))
        toastAndLog('openProjectError', error, smartAsset.value.title)
        setProjectState(backendModule.ProjectState.closed)
      }
    },
    [
      smartAsset,
      isCloud,
      projectState,
      closeProjectAbortController,
      session,
      toastAndLog,
      /* should never change */ setProjectState,
      /* should never change */ setItem,
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
      const newSpinnerState = isCloud
        ? REMOTE_SPINNER_STATE[projectState]
        : LOCAL_SPINNER_STATE[projectState]
      setSpinnerState(newSpinnerState)
      onSpinnerStateChange?.(
        projectState === backendModule.ProjectState.closed ? null : newSpinnerState
      )
    })
  }, [projectState, isCloud, onSpinnerStateChange])

  React.useEffect(() => {
    onSpinnerStateChange?.(spinner.SpinnerState.initial)
    return () => {
      onSpinnerStateChange?.(null)
    }
  }, [onSpinnerStateChange])

  eventHooks.useEventHandler(assetEvents, event => {
    switch (event.type) {
      case AssetEventType.copy:
      case AssetEventType.updateFiles:
      case AssetEventType.cut:
      case AssetEventType.cancelCut:
      case AssetEventType.move:
      case AssetEventType.delete:
      case AssetEventType.deleteForever:
      case AssetEventType.restore:
      case AssetEventType.download:
      case AssetEventType.downloadSelected:
      case AssetEventType.removeSelf:
      case AssetEventType.temporarilyAddLabels:
      case AssetEventType.temporarilyRemoveLabels:
      case AssetEventType.addLabels:
      case AssetEventType.removeLabels:
      case AssetEventType.deleteLabel: {
        // Ignored. Any missing project-related events should be handled by `ProjectNameColumn`.
        // `delete`, `deleteForever`, `restore`, `download`, and `downloadSelected`
        // are handled by`AssetRow`.
        break
      }
      case AssetEventType.openProject: {
        if (event.id !== asset.id) {
          if (!event.runInBackground && !isRunningInBackground) {
            setShouldOpenWhenReady(false)
            if (!isOtherUserUsingProject && backendModule.IS_OPENING_OR_OPENED[projectState]) {
              // void closeProject(false)
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
    }
  })

  React.useEffect(() => {
    if (shouldOpenWhenReady && projectState === backendModule.ProjectState.opened) {
      doOpenEditor(shouldSwitchPage)
      setShouldOpenWhenReady(false)
    }
    // `doOpenEditor` is a callback, not a dependency.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [shouldOpenWhenReady, shouldSwitchPage, projectState])

  const closeProject = async (triggerOnClose = true) => {
    if (triggerOnClose) {
      doCloseEditor()
    }
    setToastId(null)
    setShouldOpenWhenReady(false)
    setProjectState(backendModule.ProjectState.closing)
    onSpinnerStateChange?.(null)
    setOnSpinnerStateChange(null)
    openProjectAbortController?.abort()
    setOpenProjectAbortController(null)
    const abortController = new AbortController()
    setCloseProjectAbortController(abortController)
    if (backendModule.IS_OPENING_OR_OPENED[projectState]) {
      try {
        if (!isCloud && projectState === backendModule.ProjectState.openInProgress) {
          // Projects that are not opened cannot be closed.
          // This is the only way to wait until the project is open.
          await smartAsset.open()
        }
        try {
          await smartAsset.close()
        } catch {
          // Ignored. The project is already closed.
        }
      } finally {
        if (!abortController.signal.aborted) {
          setProjectState(backendModule.ProjectState.closed)
        }
      }
    }
  }

  switch (projectState) {
    case null:
    case backendModule.ProjectState.created:
    case backendModule.ProjectState.new:
    case backendModule.ProjectState.closing:
    case backendModule.ProjectState.closed:
      return (
        <UnstyledButton
          className="size-project-icon rounded-full"
          onPress={() => {
            unsetModal()
            doOpenManually(asset.id)
          }}
        >
          <SvgMask alt={getText('openInEditor')} src={PlayIcon} className="size-project-icon" />
        </UnstyledButton>
      )
    case backendModule.ProjectState.openInProgress:
    case backendModule.ProjectState.scheduled:
    case backendModule.ProjectState.provisioned:
    case backendModule.ProjectState.placeholder:
      return (
        <UnstyledButton
          isDisabled={isOtherUserUsingProject}
          {...(isOtherUserUsingProject ? { title: 'Someone else is using this project.' } : {})}
          className="size-project-icon rounded-full selectable enabled:active"
          onPress={() => {
            unsetModal()
            void closeProject(!isRunningInBackground)
          }}
        >
          <div className={`relative h ${isRunningInBackground ? 'text-green' : ''}`}>
            <Spinner size={ICON_SIZE_PX} state={spinnerState} />
          </div>
          <SvgMask
            alt={getText('stopExecution')}
            src={StopIcon}
            className={`size-project-icon ${isRunningInBackground ? 'text-green' : ''}`}
          />
        </UnstyledButton>
      )
    case backendModule.ProjectState.opened:
      return (
        <div>
          <UnstyledButton
            isDisabled={isOtherUserUsingProject}
            {...(isOtherUserUsingProject ? { title: 'Someone else has this project open.' } : {})}
            className="size-project-icon rounded-full selectable enabled:active"
            onPress={() => {
              unsetModal()
              void closeProject(!isRunningInBackground)
            }}
          >
            <div className={`relative h ${isRunningInBackground ? 'text-green' : ''}`}>
              <Spinner className="size-project-icon" state={spinnerState} />
            </div>
            <SvgMask
              alt={getText('stopExecution')}
              src={StopIcon}
              className={`size-project-icon ${isRunningInBackground ? 'text-green' : ''}`}
            />
          </UnstyledButton>
          {!isOtherUserUsingProject && !isRunningInBackground && (
            <UnstyledButton
              className="size-project-icon rounded-full"
              onPress={() => {
                unsetModal()
                doOpenEditor(true)
              }}
            >
              <SvgMask
                alt={getText('openInEditor')}
                src={ArrowUpIcon}
                className="size-project-icon"
              />
            </UnstyledButton>
          )}
        </div>
      )
  }
}
