/** @file An interactive button indicating the status of a project. */
import * as reactQuery from '@tanstack/react-query'

import ArrowUpIcon from '#/assets/arrow_up.svg'
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

import { useBackendQuery } from '#/hooks/backendHooks'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import { useMemo } from 'react'

import { useEventCallback } from '#/hooks/eventCallbackHooks'

// =================
// === Constants ===
// =================

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

// ===================
// === ProjectIcon ===
// ===================

/** Props for a {@link ProjectIcon}. */
export interface ProjectIconProps {
  readonly isPlaceholder: boolean
  readonly backend: Backend
  readonly isDisabled: boolean
  readonly isOpened: boolean
  readonly item: backendModule.ProjectAsset
}

/** An interactive icon indicating the status of a project. */
export default function ProjectIcon(props: ProjectIconProps) {
  const { backend, item, isOpened, isDisabled: isDisabledRaw, isPlaceholder } = props

  const isUnconditionallyDisabled = !projectHooks.useCanOpenProjects()

  const isDisabled = isDisabledRaw || isUnconditionallyDisabled

  const openProject = projectHooks.useOpenProject()
  const closeProject = projectHooks.useCloseProject()
  const openProjectTab = projectHooks.useOpenEditor()

  const { user } = authProvider.useFullUserSession()
  const { getText } = textProvider.useText()

  // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
  const itemProjectState = item.projectState ?? CLOSED_PROJECT_STATE
  const { data: projectState, isError } = reactQuery.useQuery({
    ...projectHooks.createGetProjectDetailsQuery({
      assetId: item.id,
      backend,
    }),
    select: (data) => data.state,
    enabled: !isPlaceholder && isOpened && !isUnconditionallyDisabled,
  })

  const status = projectState?.type
  const isRunningInBackground = projectState?.executeAsync ?? false

  const isCloud = backend.type === backendModule.BackendType.remote

  const isOtherUserUsingProject =
    isCloud && itemProjectState.openedBy != null && itemProjectState.openedBy !== user.email

  const { data: users } = useBackendQuery(backend, 'listUsers', [], {
    enabled: isOtherUserUsingProject,
  })

  const userOpeningProject = useMemo(
    () =>
      !isOtherUserUsingProject ? null : (
        users?.find((otherUser) => otherUser.email === itemProjectState.openedBy)
      ),
    [isOtherUserUsingProject, itemProjectState.openedBy, users],
  )

  const userOpeningProjectTooltip =
    userOpeningProject == null ? null : getText('xIsUsingTheProject', userOpeningProject.name)
  const disabledTooltip = isUnconditionallyDisabled ? getText('downloadToOpenWorkflow') : null

  const state = (() => {
    if (!isOpened && !isPlaceholder) {
      return backendModule.ProjectState.closed
    }
    // Project is closed, show open button
    if (!isOpened) {
      return (projectState ?? itemProjectState).type
    }

    if (status == null) {
      // Project is opened, but not yet queried.
      return backendModule.ProjectState.openInProgress
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
    } else if (isError) {
      return 'initial'
    } else if (status == null) {
      return 'loading-slow'
    } else {
      return backend.type === backendModule.BackendType.remote ?
          REMOTE_SPINNER_STATE[status]
        : LOCAL_SPINNER_STATE[status]
    }
  })()

  const doOpenProject = useEventCallback(() => {
    openProject({ ...item, type: backend.type })
  })
  const doCloseProject = useEventCallback(() => {
    closeProject({ ...item, type: backend.type })
  })
  const doOpenProjectTab = useEventCallback(() => {
    openProjectTab(item.id)
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
          isDisabled={isDisabled || projectState?.type === backendModule.ProjectState.closing}
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

          {!isOtherUserUsingProject && !isRunningInBackground && (
            <ariaComponents.Button
              size="large"
              variant="icon"
              extraClickZone="xsmall"
              icon={ArrowUpIcon}
              aria-label={getTooltip(getText('openInEditor'))}
              isDisabled={isDisabled}
              tooltipPlacement="right"
              onPress={doOpenProjectTab}
              testId="switch-to-project"
            />
          )}
        </div>
      )
  }
}
