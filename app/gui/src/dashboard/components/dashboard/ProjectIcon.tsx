/** @file An interactive button indicating the status of a project. */

import PlayIcon from '#/assets/play.svg'
import StopIcon from '#/assets/stop.svg'

import * as projectHooks from '#/hooks/projectHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import { Spinner } from '#/components/Spinner'
import { StatelessSpinner, type SpinnerState } from '#/components/StatelessSpinner'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'

import * as tailwindMerge from '#/utilities/tailwindMerge'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type { LaunchedProject } from '../../providers/ProjectsProvider'

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

  const isDisabled = isDisabledRaw || isUnconditionallyDisabled

  const { user } = authProvider.useFullUserSession()
  const { getText } = textProvider.useText()

  // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
  const projectState = item.projectState ?? CLOSED_PROJECT_STATE

  const status = projectState.type
  const isRunningInBackground = projectState.executeAsync ?? false

  const isOtherUserUsingProject =
    projectState.openedBy != null && projectState.openedBy !== user.email

  const userOpeningProjectTooltip =
    isOtherUserUsingProject ? getText('xIsUsingTheProject', projectState.openedBy) : null
  const disabledTooltip = isUnconditionallyDisabled ? getText('downloadToOpenWorkflow') : null

  const state = (() => {
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

  const spinnerState = ((): SpinnerState => {
    if (!isOpened) {
      return 'loading-slow'
    }

    return backend.type === backendModule.BackendType.remote ?
        REMOTE_SPINNER_STATE[status]
      : LOCAL_SPINNER_STATE[status]
  })()

  const doOpenProject = useEventCallback(async () => {
    await openProject(item.id)
  })

  const doCloseProject = useEventCallback(async () => {
    await closeProject({ ...item, type: backend.type })
  })

  const getTooltip = (defaultTooltip: string) =>
    disabledTooltip ?? userOpeningProjectTooltip ?? defaultTooltip

  switch (state) {
    case backendModule.ProjectState.new:
    case backendModule.ProjectState.closing:
    case backendModule.ProjectState.closed:
    case backendModule.ProjectState.created:
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
    case backendModule.ProjectState.openInProgress:
    case backendModule.ProjectState.scheduled:
    case backendModule.ProjectState.provisioned:
    case backendModule.ProjectState.placeholder:
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
            {...(isOtherUserUsingProject ? { title: getText('otherUserIsUsingProjectError') } : {})}
            onPress={doCloseProject}
            testId="stop-project"
          />
          <StatelessSpinner
            state={spinnerState}
            className={tailwindMerge.twJoin(
              'pointer-events-none absolute inset-0',
              isRunningInBackground && 'text-green',
            )}
          />
        </div>
      )
    case backendModule.ProjectState.opened:
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
              state="done"
              className={tailwindMerge.twMerge(
                'pointer-events-none absolute inset-0',
                isRunningInBackground && 'text-green',
              )}
            />
          </div>
        </div>
      )
  }
}
