/** @file An interactive button indicating the status of a project. */
import PlayIcon from '#/assets/play.svg'
import StopIcon from '#/assets/stop.svg'
import { Button } from '#/components/Button'
import { Spinner } from '#/components/Spinner'
import { StatelessSpinner, type SpinnerState } from '#/components/StatelessSpinner'
import { useCanRunProjects } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type Backend from '#/services/Backend'
import {
  IS_OPENING,
  IS_OPENING_OR_OPENED,
  type ProjectAsset,
  type ProjectId,
  type ProjectState,
} from '#/services/Backend'
import { twJoin, twMerge } from '#/utilities/tailwindMerge'
import type { LaunchedProject } from '$/providers/container'
import { useFullUserSession, useText } from '$/providers/react'
import {
  useAreOtherProjectsOpening,
  useIsProjectClosing,
  useIsProjectOpening,
  useLaunchedProject,
} from '$/providers/react/container'

// eslint-disable-next-line react-refresh/only-export-components
export const CLOSED_PROJECT_STATE = { type: 'Closed' } as const

/* eslint-disable @typescript-eslint/naming-convention */

/**
 * The corresponding {@link SpinnerState} for each {@link ProjectState},
 * when using the remote backend.
 */
const REMOTE_SPINNER_STATE: Readonly<Record<ProjectState, SpinnerState>> = {
  Closed: 'loading-slow',
  Created: 'loading-slow',
  New: 'loading-slow',
  Placeholder: 'loading-slow',
  OpenInProgress: 'loading-slow',
  HybridOpenInProgress: 'loading-slow',
  Provisioned: 'loading-slow',
  Scheduled: 'loading-slow',
  Opened: 'done',
  HybridOpened: 'done',
}
/**
 * The corresponding {@link SpinnerState} for each {@link ProjectState},
 * when using the local backend.
 */
const LOCAL_SPINNER_STATE: Readonly<Record<ProjectState, SpinnerState>> = {
  Closed: 'loading-slow',
  Created: 'loading-slow',
  New: 'loading-slow',
  Placeholder: 'loading-medium',
  OpenInProgress: 'loading-slow',
  HybridOpenInProgress: 'loading-slow',
  Provisioned: 'loading-medium',
  Scheduled: 'loading-medium',
  Opened: 'done',
  HybridOpened: 'done',
}

/* eslint-enable @typescript-eslint/naming-convention */

/** Props for a {@link ProjectIcon}. */
export interface ProjectIconProps {
  readonly isPlaceholder: boolean
  readonly backend: Backend
  readonly isDisabled: boolean
  readonly isOpened: boolean
  readonly item: ProjectAsset
  readonly closeProject: (project: LaunchedProject) => Promise<void>
  readonly openProject: (projectId: ProjectId) => Promise<void>
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

  const launched = useLaunchedProject(item.id)
  const isUnconditionallyDisabled = !useCanRunProjects().locally[backend.type]

  const { user } = useFullUserSession()
  const { getText } = useText()

  // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
  const projectState = item.projectState ?? CLOSED_PROJECT_STATE

  const status = projectState.type
  const isRunningInBackground = projectState.executeAsync ?? false

  const isOtherUserUsingProject =
    projectState.openedBy != null && projectState.openedBy !== user.email

  const isProjectOpening = useIsProjectOpening(item.id)
  const isProjectClosing = useIsProjectClosing(item.id)

  const state = (() => {
    if (isProjectOpening) {
      return 'OpenInProgress'
    }

    if (!isOpened && !isPlaceholder) {
      return 'Closed'
    }
    // Project is closed, show open button
    if (!isOpened) {
      return projectState.type
    }

    if (status === 'Closed') {
      // Project is opened locally, but not on the backend yet.
      return 'OpenInProgress'
    }
    return status
  })()

  const areOtherProjectsOpening = useAreOtherProjectsOpening(item.id)
  const isAnotherProjectOpening = areOtherProjectsOpening && !IS_OPENING_OR_OPENED[state]
  const isDisabled =
    isDisabledRaw || isUnconditionallyDisabled || isAnotherProjectOpening || isProjectClosing

  const userOpeningProjectTooltip =
    isOtherUserUsingProject ? getText('xIsUsingTheProject', projectState.openedBy) : null
  const disabledTooltip = isUnconditionallyDisabled ? getText('downloadToOpenWorkflow') : null
  const anotherProjectOpeningTooltip =
    isAnotherProjectOpening ? getText('anotherProjectIsBeingOpenedError') : null
  const closingProjectTooltip = isProjectClosing ? getText('syncingProjectFiles') : null

  const spinnerState =
    !isOpened ? 'loading-slow'
    : backend.type === 'remote' ? REMOTE_SPINNER_STATE[status]
    : LOCAL_SPINNER_STATE[status]

  const doOpenProject = useEventCallback(() => {
    // The "open project" icon should never be in the loading state.
    void openProject(item.id)
  })

  const doCloseProject = useEventCallback(() => {
    if (launched != null) {
      // This may be a hybrid project; use "launched" information to close properly.
      return closeProject(launched)
    } else {
      return closeProject({ ...item, type: backend.type })
    }
  })

  const getTooltip = (defaultTooltip: string) =>
    disabledTooltip ??
    userOpeningProjectTooltip ??
    anotherProjectOpeningTooltip ??
    closingProjectTooltip ??
    defaultTooltip

  // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
  switch (true) {
    case IS_OPENING[state]:
      return (
        <div className="relative flex">
          <Button
            size="large"
            variant="icon"
            extraClickZone="xsmall"
            isDisabled={isDisabled || isOtherUserUsingProject}
            icon={StopIcon}
            aria-label={getTooltip(getText('stopExecution'))}
            tooltipPlacement="left"
            className={twJoin(isRunningInBackground && 'text-green')}
            onPress={doCloseProject}
            testId="stop-project"
          />
          <StatelessSpinner
            phase={spinnerState}
            className={twJoin(
              'pointer-events-none absolute inset-0',
              isRunningInBackground && 'text-green',
            )}
          />
        </div>
      )
    case IS_OPENING_OR_OPENED[state]:
      return (
        <div className="flex flex-row gap-0.5">
          <div className="relative flex">
            <Button
              size="large"
              variant="icon"
              extraClickZone="xsmall"
              isDisabled={isDisabled || isOtherUserUsingProject}
              icon={StopIcon}
              aria-label={getTooltip(getText('stopExecution'))}
              tooltipPlacement="left"
              className={twJoin(isRunningInBackground && 'text-green')}
              onPress={doCloseProject}
              testId="stop-project"
            />
            <Spinner
              phase="done"
              className={twMerge(
                'pointer-events-none absolute inset-0',
                isRunningInBackground && 'text-green',
              )}
            />
          </div>
        </div>
      )
    default:
      return (
        <Button
          size="large"
          variant="icon"
          icon={PlayIcon}
          aria-label={getTooltip(getText('openInEditor'))}
          tooltipPlacement="left"
          extraClickZone="xsmall"
          isDisabled={isDisabled}
          className="shrink-0"
          onPress={doOpenProject}
          testId="open-project"
        />
      )
  }
}
