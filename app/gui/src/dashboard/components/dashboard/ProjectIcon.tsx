/** @file An interactive button indicating the status of a project. */
import * as reactQuery from '@tanstack/react-query'

import ArrowUpIcon from '#/assets/arrow_up.svg'
import PlayIcon from '#/assets/play.svg'
import StopIcon from '#/assets/stop.svg'

import * as projectHooks from '#/hooks/projectHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import Spinner from '#/components/Spinner'
import StatelessSpinner, * as spinner from '#/components/StatelessSpinner'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'

import { useBackendQuery } from '#/hooks/backendHooks'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import { useMemo } from 'react'

// =================
// === Constants ===
// =================

export const CLOSED_PROJECT_STATE = { type: backendModule.ProjectState.closed } as const

/**
 * The corresponding {@link spinner.SpinnerState} for each {@link backendModule.ProjectState},
 * when using the remote backend.
 */
const REMOTE_SPINNER_STATE: Readonly<Record<backendModule.ProjectState, spinner.SpinnerState>> = {
  [backendModule.ProjectState.closed]: spinner.SpinnerState.loadingSlow,
  [backendModule.ProjectState.closing]: spinner.SpinnerState.loadingMedium,
  [backendModule.ProjectState.created]: spinner.SpinnerState.loadingSlow,
  [backendModule.ProjectState.new]: spinner.SpinnerState.loadingSlow,
  [backendModule.ProjectState.placeholder]: spinner.SpinnerState.loadingSlow,
  [backendModule.ProjectState.openInProgress]: spinner.SpinnerState.loadingSlow,
  [backendModule.ProjectState.provisioned]: spinner.SpinnerState.loadingSlow,
  [backendModule.ProjectState.scheduled]: spinner.SpinnerState.loadingSlow,
  [backendModule.ProjectState.opened]: spinner.SpinnerState.done,
}
/**
 * The corresponding {@link spinner.SpinnerState} for each {@link backendModule.ProjectState},
 * when using the local backend.
 */
const LOCAL_SPINNER_STATE: Readonly<Record<backendModule.ProjectState, spinner.SpinnerState>> = {
  [backendModule.ProjectState.closed]: spinner.SpinnerState.loadingSlow,
  [backendModule.ProjectState.closing]: spinner.SpinnerState.loadingMedium,
  [backendModule.ProjectState.created]: spinner.SpinnerState.loadingSlow,
  [backendModule.ProjectState.new]: spinner.SpinnerState.loadingSlow,
  [backendModule.ProjectState.placeholder]: spinner.SpinnerState.loadingMedium,
  [backendModule.ProjectState.openInProgress]: spinner.SpinnerState.loadingSlow,
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
  readonly isDisabled: boolean
  readonly isOpened: boolean
  readonly item: backendModule.ProjectAsset
}

/** An interactive icon indicating the status of a project. */
export default function ProjectIcon(props: ProjectIconProps) {
  const { backend, item, isOpened, isDisabled } = props

  const openProject = projectHooks.useOpenProject()
  const closeProject = projectHooks.useCloseProject()
  const openProjectTab = projectHooks.useOpenEditor()

  const { user } = authProvider.useFullUserSession()
  const { getText } = textProvider.useText()

  // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
  const itemProjectState = item.projectState ?? CLOSED_PROJECT_STATE
  const { data: projectState, isError } = reactQuery.useQuery({
    ...projectHooks.createGetProjectDetailsQuery.createPassiveListener(item.id),
    select: (data) => data?.state,
    enabled: isOpened,
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

  const state = (() => {
    // Project is closed, show open button
    if (!isOpened) {
      return (projectState ?? itemProjectState).type
    } else if (status == null) {
      // Project is opened, but not yet queried.
      return backendModule.ProjectState.openInProgress
    } else if (status === backendModule.ProjectState.closed) {
      // Project is opened locally, but not on the backend yet.
      return backendModule.ProjectState.openInProgress
    } else {
      return status
    }
  })()

  const spinnerState = (() => {
    if (!isOpened) {
      return spinner.SpinnerState.initial
    } else if (isError) {
      return spinner.SpinnerState.initial
    } else if (status == null) {
      return spinner.SpinnerState.loadingSlow
    } else {
      return backend.type === backendModule.BackendType.remote ?
          REMOTE_SPINNER_STATE[status]
        : LOCAL_SPINNER_STATE[status]
    }
  })()

  const doOpenProject = () => {
    openProject({ ...item, type: backend.type })
  }
  const doCloseProject = () => {
    closeProject({ ...item, type: backend.type })
  }
  const doOpenProjectTab = () => {
    openProjectTab(item.id)
  }

  switch (state) {
    case backendModule.ProjectState.new:
    case backendModule.ProjectState.closing:
    case backendModule.ProjectState.closed:
    case backendModule.ProjectState.created:
      return (
        <ariaComponents.Button
          size="custom"
          variant="icon"
          icon={PlayIcon}
          aria-label={getText('openInEditor')}
          tooltipPlacement="left"
          extraClickZone="xsmall"
          isDisabled={isDisabled || projectState?.type === backendModule.ProjectState.closing}
          onPress={doOpenProject}
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
            aria-label={userOpeningProjectTooltip ?? getText('stopExecution')}
            tooltipPlacement="left"
            className={tailwindMerge.twJoin(isRunningInBackground && 'text-green')}
            {...(isOtherUserUsingProject ? { title: getText('otherUserIsUsingProjectError') } : {})}
            onPress={doCloseProject}
          />
          <StatelessSpinner
            state={spinnerState}
            className={tailwindMerge.twMerge(
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
              aria-label={userOpeningProjectTooltip ?? getText('stopExecution')}
              tooltipPlacement="left"
              className={tailwindMerge.twMerge(isRunningInBackground && 'text-green')}
              onPress={doCloseProject}
            />
            <Spinner
              state={spinner.SpinnerState.done}
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
              aria-label={userOpeningProjectTooltip ?? getText('openInEditor')}
              isDisabled={isDisabled}
              tooltipPlacement="right"
              onPress={doOpenProjectTab}
            />
          )}
        </div>
      )
  }
}
