/** @file An interactive button indicating the status of a project. */

import PlayIcon from '#/assets/play.svg'
import StopIcon from '#/assets/stop.svg'

import * as projectHooks from '#/hooks/projectHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import { StatelessSpinner, type SpinnerState } from '#/components/StatelessSpinner'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'

import * as tailwindMerge from '#/utilities/tailwindMerge'

import { Spinner } from '#/components/Spinner'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useStore } from '#/hooks/storeHooks'
import type { LaunchedProject } from '#/providers/ProjectsProvider'
import { projectsStore } from '#/providers/ProjectsProvider/hooks'

// eslint-disable-next-line react-refresh/only-export-components
export const CLOSED_PROJECT_STATE = { type: backendModule.ProjectState.closed } as const

/**
 * The corresponding {@link SpinnerState} for each {@link backendModule.ProjectState},
 * when using the remote backend.
 */
const REMOTE_SPINNER_STATE: Readonly<Record<backendModule.ProjectState, SpinnerState>> = {
  [backendModule.ProjectState.closed]: 'loading-slow',
  [backendModule.ProjectState.closing]: 'loading-medium',
  [backendModule.ProjectState.created]: 'loading-slow',
  [backendModule.ProjectState.new]: 'loading-slow',
  [backendModule.ProjectState.placeholder]: 'loading-slow',
  [backendModule.ProjectState.openInProgress]: 'loading-slow',
  [backendModule.ProjectState.provisioned]: 'loading-slow',
  [backendModule.ProjectState.scheduled]: 'loading-slow',
  [backendModule.ProjectState.opened]: 'done',
}
/**
 * The corresponding {@link SpinnerState} for each {@link backendModule.ProjectState},
 * when using the local backend.
 */
const LOCAL_SPINNER_STATE: Readonly<Record<backendModule.ProjectState, SpinnerState>> = {
  [backendModule.ProjectState.closed]: 'loading-slow',
  [backendModule.ProjectState.closing]: 'loading-medium',
  [backendModule.ProjectState.created]: 'loading-slow',
  [backendModule.ProjectState.new]: 'loading-slow',
  [backendModule.ProjectState.placeholder]: 'loading-medium',
  [backendModule.ProjectState.openInProgress]: 'loading-slow',
  [backendModule.ProjectState.provisioned]: 'loading-medium',
  [backendModule.ProjectState.scheduled]: 'loading-medium',
  [backendModule.ProjectState.opened]: 'done',
}

/** Props for a {@link ProjectIcon}. */
export interface ProjectIconProps {
  readonly isPlaceholder: boolean
  readonly backend: Backend
  readonly isDisabled: boolean
  readonly isOpened: boolean
  readonly item: backendModule.ProjectAsset
  readonly closeProject: (project: LaunchedProject) => Promise<void>
  readonly openProject: (projectId: backendModule.ProjectId) => Promise<void>
}

/** An interactive icon indicating the status of a project. */
export default function ProjectIcon(props: ProjectIconProps) {
  const {
    backend,
    item,
    isOpened,
    isDisabled: isDisabledRaw,
    isPlaceholder,
    closeProject,
    openProject,
  } = props

  const isUnconditionallyDisabled = !projectHooks.useCanOpenProjects()

  const { user } = authProvider.useFullUserSession()
  const { getText } = textProvider.useText()

  // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
  const projectState = item.projectState ?? CLOSED_PROJECT_STATE

  const status = projectState.type
  const isRunningInBackground = projectState.executeAsync ?? false

  const isOtherUserUsingProject =
    projectState.openedBy != null && projectState.openedBy !== user.email

  const isProjectOpening = useStore(
    projectsStore,
    ({ openingProjects }) => openingProjects.has(item.id),
    { unsafeEnableTransition: true },
  )

  const state = (() => {
    if (isProjectOpening) {
      return backendModule.ProjectState.openInProgress
    }

    if (!isOpened && !isPlaceholder) {
      return backendModule.ProjectState.closed
    }
    // Project is closed, show open button
    if (!isOpened) {
      return projectState.type
    }

    if (status === backendModule.ProjectState.closed) {
      // Project is opened locally, but not on the backend yet.
      return backendModule.ProjectState.openInProgress
    }
    return status
  })()

  const areOtherProjectsOpening = useStore(
    projectsStore,
    ({ openingProjects }) => openingProjects.size !== 0 && !openingProjects.has(item.id),
    { unsafeEnableTransition: true },
  )
  const isAnotherProjectOpening =
    areOtherProjectsOpening && !backendModule.IS_OPENING_OR_OPENED[state]
  const isDisabled = isDisabledRaw || isUnconditionallyDisabled || isAnotherProjectOpening

  const userOpeningProjectTooltip =
    isOtherUserUsingProject ? getText('xIsUsingTheProject', projectState.openedBy) : null
  const disabledTooltip = isUnconditionallyDisabled ? getText('downloadToOpenWorkflow') : null
  const anotherProjectOpeningTooltip =
    isAnotherProjectOpening ? getText('anotherProjectIsBeingOpenedError') : null

  const spinnerState = ((): SpinnerState => {
    if (!isOpened) {
      return 'loading-slow'
    }

    return backend.type === backendModule.BackendType.remote ?
        REMOTE_SPINNER_STATE[status]
      : LOCAL_SPINNER_STATE[status]
  })()

  const doOpenProject = useEventCallback(() => {
    // The "open project" icon should never be in the loading state.
    void openProject(item.id)
  })

  const doCloseProject = useEventCallback(async () => {
    await closeProject({ ...item, type: backend.type })
  })

  const getTooltip = (defaultTooltip: string) =>
    disabledTooltip ?? userOpeningProjectTooltip ?? anotherProjectOpeningTooltip ?? defaultTooltip

  // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
  switch (true) {
    case backendModule.IS_OPENING[state]:
      return (
        <div className="relative flex">
          <ariaComponents.Button
            size="large"
            variant="icon"
            extraClickZone="xsmall"
            isDisabled={isDisabled || isOtherUserUsingProject}
            icon={StopIcon}
            aria-label={getTooltip(getText('stopExecution'))}
            tooltipPlacement="left"
            className={tailwindMerge.twJoin(isRunningInBackground && 'text-green')}
            onPress={doCloseProject}
            testId="stop-project"
          />
          <StatelessSpinner
            phase={spinnerState}
            className={tailwindMerge.twJoin(
              'pointer-events-none absolute inset-0',
              isRunningInBackground && 'text-green',
            )}
          />
        </div>
      )
    case backendModule.IS_OPENING_OR_OPENED[state]:
      return (
        <div className="flex flex-row gap-0.5">
          <div className="relative flex">
            <ariaComponents.Button
              size="large"
              variant="icon"
              extraClickZone="xsmall"
              isDisabled={isDisabled || isOtherUserUsingProject}
              icon={StopIcon}
              aria-label={getTooltip(getText('stopExecution'))}
              tooltipPlacement="left"
              className={tailwindMerge.twJoin(isRunningInBackground && 'text-green')}
              onPress={doCloseProject}
              testId="stop-project"
            />
            <Spinner
              phase="done"
              className={tailwindMerge.twMerge(
                'pointer-events-none absolute inset-0',
                isRunningInBackground && 'text-green',
              )}
            />
          </div>
        </div>
      )
    default:
      return (
        <ariaComponents.Button
          size="large"
          variant="icon"
          icon={PlayIcon}
          aria-label={getTooltip(getText('openInEditor'))}
          tooltipPlacement="left"
          extraClickZone="xsmall"
          isDisabled={isDisabled || projectState.type === backendModule.ProjectState.closing}
          className="shrink-0"
          onPress={doOpenProject}
          testId="open-project"
        />
      )
  }
}
