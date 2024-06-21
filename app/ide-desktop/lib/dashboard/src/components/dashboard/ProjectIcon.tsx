/** @file An interactive button indicating the status of a project. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import * as toast from 'react-toastify'

import ArrowUpIcon from 'enso-assets/arrow_up.svg'
import PlayIcon from 'enso-assets/play.svg'
import StopIcon from 'enso-assets/stop.svg'

import * as backendHooks from '#/hooks/backendHooks'
import * as eventHooks from '#/hooks/eventHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as sessionProvider from '#/providers/SessionProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'

import * as ariaComponents from '#/components/AriaComponents'
import Spinner, * as spinner from '#/components/Spinner'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import * as object from '#/utilities/object'
import * as tailwindMerge from '#/utilities/tailwindMerge'

// =================
// === Constants ===
// =================

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
  readonly backend: Backend
  readonly item: backendModule.ProjectAsset
  readonly setItem: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>
  readonly assetEvents: assetEvent.AssetEvent[]
  readonly dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
  readonly setProjectStartupInfo: (projectStartupInfo: backendModule.ProjectStartupInfo) => void
  readonly doCloseEditor: () => void
  readonly doOpenEditor: (switchPage: boolean) => void
}

/** An interactive icon indicating the status of a project. */
export default function ProjectIcon(props: ProjectIconProps) {
  const { backend, item, setItem, assetEvents, setProjectStartupInfo, dispatchAssetEvent } = props
  const { doCloseEditor, doOpenEditor } = props
  const { session } = sessionProvider.useSession()
  const { user } = authProvider.useNonPartialUserSession()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { getText } = textProvider.useText()
  const state = item.projectState.type
  const setState = React.useCallback(
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
          newProjectState = object.omit(newProjectState, 'openedBy')
        } else {
          newProjectState = object.merge(newProjectState, {
            openedBy: user.email,
          })
        }
        return object.merge(oldItem, { projectState: newProjectState })
      })
    },
    [user, setItem]
  )
  const [spinnerState, setSpinnerState] = React.useState(spinner.SpinnerState.initial)
  const [shouldOpenWhenReady, setShouldOpenWhenReady] = React.useState(false)
  const [isRunningInBackground, setIsRunningInBackground] = React.useState(
    item.projectState.executeAsync ?? false
  )
  const [shouldSwitchPage, setShouldSwitchPage] = React.useState(false)
  const doOpenEditorRef = React.useRef(doOpenEditor)
  doOpenEditorRef.current = doOpenEditor
  const toastId: toast.Id = React.useId()
  const isOpening =
    backendModule.IS_OPENING[item.projectState.type] &&
    item.projectState.type !== backendModule.ProjectState.placeholder
  const isCloud = backend.type === backendModule.BackendType.remote
  const isOtherUserUsingProject =
    isCloud && item.projectState.openedBy != null && item.projectState.openedBy !== user.email

  const openProjectMutation = backendHooks.useBackendMutation(backend, 'openProject')
  const closeProjectMutation = backendHooks.useBackendMutation(backend, 'closeProject')
  const getProjectDetailsMutation = backendHooks.useBackendMutation(backend, 'getProjectDetails')
  const waitUntilProjectIsReadyMutation = backendHooks.useBackendMutation(
    backend,
    'waitUntilProjectIsReady'
  )
  const openProjectMutate = openProjectMutation.mutateAsync
  const getProjectDetailsMutate = getProjectDetailsMutation.mutateAsync

  const openProject = React.useCallback(
    async (shouldRunInBackground: boolean) => {
      if (state !== backendModule.ProjectState.opened) {
        setState(backendModule.ProjectState.openInProgress)
        try {
          await openProjectMutate([
            item.id,
            {
              executeAsync: shouldRunInBackground,
              parentId: item.parentId,
              cognitoCredentials: session,
            },
            item.title,
          ])
        } catch (error) {
          const project = await getProjectDetailsMutate([item.id, item.parentId, item.title])
          // `setState` is not used here as `project` contains the full state information,
          // not just the state type.
          setItem(object.merger({ projectState: project.state }))
          toastAndLog('openProjectError', error, item.title)
          setState(backendModule.ProjectState.closed)
        }
      }
    },
    [
      state,
      item,
      session,
      toastAndLog,
      openProjectMutate,
      getProjectDetailsMutate,
      setState,
      setItem,
    ]
  )

  const openEditorMutation = reactQuery.useMutation({
    mutationKey: ['openEditor', item.id],
    networkMode: 'always',
    mutationFn: async (abortController: AbortController) => {
      if (!isRunningInBackground && isCloud) {
        toast.toast.loading(LOADING_MESSAGE, { toastId })
      }
      const project = await waitUntilProjectIsReadyMutation.mutateAsync([
        item.id,
        item.parentId,
        item.title,
        abortController,
      ])
      setProjectStartupInfo({
        project,
        projectAsset: item,
        setProjectAsset: setItem,
        backendType: backend.type,
        accessToken: session?.accessToken ?? null,
      })
      if (!abortController.signal.aborted) {
        toast.toast.dismiss(toastId)
        setState(backendModule.ProjectState.opened)
      }
    },
  })
  const openEditorMutate = openEditorMutation.mutate

  React.useEffect(() => {
    if (isOpening) {
      const abortController = new AbortController()
      openEditorMutate(abortController)
      return () => {
        abortController.abort()
      }
    } else {
      return
    }
  }, [isOpening, openEditorMutate])

  React.useEffect(() => {
    // Ensure that the previous spinner state is visible for at least one frame.
    requestAnimationFrame(() => {
      const newSpinnerState =
        backend.type === backendModule.BackendType.remote
          ? REMOTE_SPINNER_STATE[state]
          : LOCAL_SPINNER_STATE[state]
      setSpinnerState(newSpinnerState)
    })
  }, [state, backend.type])

  eventHooks.useEventHandler(assetEvents, event => {
    switch (event.type) {
      case AssetEventType.openProject: {
        if (event.id !== item.id) {
          if (!event.runInBackground && !isRunningInBackground) {
            setShouldOpenWhenReady(false)
            if (!isOtherUserUsingProject && backendModule.IS_OPENING_OR_OPENED[state]) {
              void closeProject()
            }
          }
        } else {
          if (backendModule.IS_OPENING_OR_OPENED[state]) {
            if (!isRunningInBackground) {
              doOpenEditor(true)
            }
          } else {
            setShouldOpenWhenReady(!event.runInBackground)
            setShouldSwitchPage(event.shouldAutomaticallySwitchPage)
            setIsRunningInBackground(event.runInBackground)
            void openProject(event.runInBackground)
          }
        }
        break
      }
      case AssetEventType.closeProject: {
        if (event.id === item.id) {
          setShouldOpenWhenReady(false)
          void closeProject()
        }
        break
      }
      default: {
        // Ignored. Any missing project-related events should be handled by `ProjectNameColumn`.
        // `delete`, `deleteForever`, `restore`, `download`, and `downloadSelected`
        // are handled by`AssetRow`.
        break
      }
    }
  })

  React.useEffect(() => {
    if (state === backendModule.ProjectState.opened) {
      if (shouldOpenWhenReady) {
        doOpenEditorRef.current(shouldSwitchPage)
        setShouldOpenWhenReady(false)
      }
    }
  }, [shouldOpenWhenReady, shouldSwitchPage, state])

  const closeProject = async () => {
    if (!isRunningInBackground) {
      doCloseEditor()
    }
    toast.toast.dismiss(toastId)
    setShouldOpenWhenReady(false)
    setState(backendModule.ProjectState.closing)
    await closeProjectMutation.mutateAsync([item.id, item.title])
  }

  switch (state) {
    case null:
    case backendModule.ProjectState.created:
    case backendModule.ProjectState.new:
    case backendModule.ProjectState.closing:
    case backendModule.ProjectState.closed:
      return (
        <ariaComponents.Button
          size="custom"
          variant="icon"
          icon={PlayIcon}
          aria-label={getText('openInEditor')}
          tooltipPlacement="left"
          className="h-6 border-0"
          onPress={() => {
            dispatchAssetEvent({
              type: AssetEventType.openProject,
              id: item.id,
              shouldAutomaticallySwitchPage: true,
              runInBackground: false,
            })
          }}
        />
      )
    case backendModule.ProjectState.openInProgress:
    case backendModule.ProjectState.scheduled:
    case backendModule.ProjectState.provisioned:
    case backendModule.ProjectState.placeholder:
      return (
        <div className="relative flex">
          <ariaComponents.Button
            size="custom"
            variant="icon"
            isDisabled={isOtherUserUsingProject}
            isActive={!isOtherUserUsingProject}
            icon={StopIcon}
            aria-label={getText('stopExecution')}
            tooltipPlacement="left"
            {...(isOtherUserUsingProject ? { title: getText('otherUserIsUsingProjectError') } : {})}
            className={tailwindMerge.twMerge('h-6 border-0', isRunningInBackground && 'text-green')}
            onPress={closeProject}
          />
          <Spinner
            state={spinnerState}
            className={tailwindMerge.twMerge(
              'pointer-events-none absolute top-0 size-project-icon',
              isRunningInBackground && 'text-green'
            )}
          />
        </div>
      )
    case backendModule.ProjectState.opened:
      return (
        <div className="flex flex-row gap-0.5">
          <div className="relative flex">
            <ariaComponents.Button
              size="custom"
              variant="icon"
              isDisabled={isOtherUserUsingProject}
              isActive={!isOtherUserUsingProject}
              icon={StopIcon}
              aria-label={getText('stopExecution')}
              tooltipPlacement="left"
              {...(isOtherUserUsingProject
                ? { title: getText('otherUserIsUsingProjectError') }
                : {})}
              className={tailwindMerge.twMerge(
                'h-6 border-0',
                isRunningInBackground && 'text-green'
              )}
              onPress={closeProject}
            />
            <Spinner
              state={spinnerState}
              className={tailwindMerge.twMerge(
                'pointer-events-none absolute top-0 size-project-icon',
                isRunningInBackground && 'text-green'
              )}
            />
          </div>
          {!isOtherUserUsingProject && !isRunningInBackground && (
            <ariaComponents.Button
              size="custom"
              variant="icon"
              icon={ArrowUpIcon}
              aria-label={getText('openInEditor')}
              tooltipPlacement="right"
              className="h-6 border-0"
              onPress={() => {
                doOpenEditor(true)
              }}
            />
          )}
        </div>
      )
  }
}
