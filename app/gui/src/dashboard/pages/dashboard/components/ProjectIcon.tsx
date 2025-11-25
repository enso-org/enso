/** @file An interactive button indicating the status of a project. */
import PlayIcon from '#/assets/play.svg'
import StopIcon from '#/assets/stop.svg'
import { Button } from '#/components/Button'
import { Spinner } from '#/components/Spinner'
import { StatelessSpinner, type SpinnerState } from '#/components/StatelessSpinner'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { twJoin, twMerge } from '#/utilities/tailwindMerge'
import { useFullUserSession, useText } from '$/providers/react'
import {
  useAreOtherProjectsOpening,
  useIsProjectClosing,
  useIsProjectOpened,
  useIsProjectOpening,
  useOpenedProjects,
} from '$/providers/react/openedProjects'
import type { Backend } from 'enso-common/src/services/Backend'
import { BackendType, ProjectState, type ProjectAsset } from 'enso-common/src/services/Backend'

// eslint-disable-next-line react-refresh/only-export-components
export const CLOSED_PROJECT_STATE = { type: ProjectState.closed } as const

/**
 * The corresponding {@link SpinnerState} for each {@link ProjectState},
 * when using the remote backend.
 */
const REMOTE_SPINNER_STATE: Readonly<Record<ProjectState, SpinnerState>> = {
  [ProjectState.closed]: 'loading-slow',
  [ProjectState.created]: 'loading-slow',
  [ProjectState.new]: 'loading-slow',
  [ProjectState.placeholder]: 'loading-slow',
  [ProjectState.openInProgress]: 'loading-slow',
  [ProjectState.hybridOpenInProgress]: 'loading-slow',
  [ProjectState.provisioned]: 'loading-slow',
  [ProjectState.scheduled]: 'loading-slow',
  [ProjectState.opened]: 'done',
  [ProjectState.hybridOpened]: 'done',
}
/**
 * The corresponding {@link SpinnerState} for each {@link ProjectState},
 * when using the local backend.
 */
const LOCAL_SPINNER_STATE: Readonly<Record<ProjectState, SpinnerState>> = {
  [ProjectState.closed]: 'loading-slow',
  [ProjectState.created]: 'loading-slow',
  [ProjectState.new]: 'loading-slow',
  [ProjectState.placeholder]: 'loading-medium',
  [ProjectState.openInProgress]: 'loading-slow',
  [ProjectState.hybridOpenInProgress]: 'loading-slow',
  [ProjectState.provisioned]: 'loading-medium',
  [ProjectState.scheduled]: 'loading-medium',
  [ProjectState.opened]: 'done',
  [ProjectState.hybridOpened]: 'done',
}

/** Props for a {@link ProjectIcon}. */
export interface ProjectIconProps {
  readonly isPlaceholder: boolean
  readonly backend: Backend
  readonly isDisabled: boolean
  readonly item: ProjectAsset
}

/** An interactive icon indicating the status of a project. */
export default function ProjectIcon(props: ProjectIconProps) {
  const { backend, item, isDisabled: isDisabledRaw } = props

  const openedProjects = useOpenedProjects()
  const isUnconditionallyDisabled = !openedProjects.canOpenProjectLocally(backend.type)

  const { user } = useFullUserSession()
  const { getText } = useText()

  // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
  const projectState = item.projectState ?? CLOSED_PROJECT_STATE

  const status = projectState.type
  const isRunningInBackground = projectState.executeAsync ?? false

  const isOtherUserUsingProject =
    projectState.openedBy != null && projectState.openedBy !== user.email

  const isProjectOpening = useIsProjectOpening(item)
  const isProjectOpened = useIsProjectOpened(item)
  const isProjectClosing = useIsProjectClosing(item.id)

  const areOtherProjectsOpening = useAreOtherProjectsOpening(item.id)
  const isAnotherProjectOpening = areOtherProjectsOpening && !isProjectOpening
  const isDisabled =
    isDisabledRaw || isUnconditionallyDisabled || isAnotherProjectOpening || isProjectClosing

  const userOpeningProjectTooltip =
    isOtherUserUsingProject ? getText('xIsUsingTheProject', projectState.openedBy) : null
  const disabledTooltip = isUnconditionallyDisabled ? getText('downloadToOpenWorkflow') : null
  const anotherProjectOpeningTooltip =
    isAnotherProjectOpening ? getText('anotherProjectIsBeingOpenedError') : null
  const closingProjectTooltip = isProjectClosing ? getText('syncingProjectFiles') : null

  const spinnerState = ((): SpinnerState => {
    return backend.type === BackendType.remote ?
        REMOTE_SPINNER_STATE[status]
      : LOCAL_SPINNER_STATE[status]
  })()

  const doOpenProject = useEventCallback(() => {
    openedProjects.openProjectLocally(item, backend.type)
  })

  const doCloseProject = useEventCallback(() => {
    return openedProjects.closeProject(item.id, { asset: item, backendType: backend.type })
  })

  const getTooltip = (defaultTooltip: string) =>
    disabledTooltip ??
    userOpeningProjectTooltip ??
    anotherProjectOpeningTooltip ??
    closingProjectTooltip ??
    defaultTooltip

  // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
  switch (true) {
    case isProjectOpening:
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
    case isProjectOpened:
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
