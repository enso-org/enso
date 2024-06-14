/** @file An interactive button indicating the status of a project. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import * as toast from 'react-toastify'
import * as tailwindMerge from 'tailwind-merge'

import ArrowUpIcon from 'enso-assets/arrow_up.svg'
import PlayIcon from 'enso-assets/play.svg'
import StopIcon from 'enso-assets/stop.svg'

import * as backendHooks from '#/hooks/backendHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as sessionProvider from '#/providers/SessionProvider'
import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import Spinner, * as spinner from '#/components/Spinner'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

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
  readonly doCloseEditor: () => void
  readonly doOpenEditor: (switchPage: boolean) => void
}

/** An interactive icon indicating the status of a project. */
export default function ProjectIcon(props: ProjectIconProps) {
  const { backend, item: itemRaw } = props
  const { doCloseEditor, doOpenEditor } = props
  const { session } = sessionProvider.useSession()
  const { user } = authProvider.useNonPartialUserSession()
  const { getText } = textProvider.useText()
  const item = backendHooks.useBackendGetAsset(backend, itemRaw.id, itemRaw.parentId) ?? itemRaw
  const state = item.projectState.type
  const [spinnerState, setSpinnerState] = React.useState(spinner.SpinnerState.initial)
  const isRunningInBackground = item.projectState.executeAsync ?? false
  const toastId: toast.Id = React.useId()
  const isOpening =
    backendModule.IS_OPENING[item.projectState.type] &&
    item.projectState.type !== backendModule.ProjectState.placeholder
  const isCloud = backend.type === backendModule.BackendType.remote
  const isOtherUserUsingProject =
    isCloud && item.projectState.openedBy != null && item.projectState.openedBy !== user?.email

  const openProjectMutation = backendHooks.useBackendMutation(backend, 'openProject')
  const closeProjectMutation = backendHooks.useBackendMutation(backend, 'closeProject')
  const waitUntilProjectIsReadyMutation = backendHooks.useBackendMutation(
    backend,
    'waitUntilProjectIsReady'
  )

  const openEditorMutation = reactQuery.useMutation({
    mutationKey: ['openEditor', item.id],
    mutationFn: async (abortController: AbortController) => {
      if (!isRunningInBackground) {
        toast.toast.loading(LOADING_MESSAGE, { toastId })
      }
      await waitUntilProjectIsReadyMutation.mutateAsync([item.id, item.parentId, abortController])
      if (!abortController.signal.aborted) {
        toast.toast.dismiss(toastId)
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

  const closeProject = async () => {
    if (!isRunningInBackground) {
      doCloseEditor()
    }
    toast.toast.dismiss(toastId)
    await closeProjectMutation.mutateAsync([item.id])
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
          variant="custom"
          noExtraClickZone
          icon={PlayIcon}
          aria-label={getText('openInEditor')}
          tooltipPlacement="left"
          className="size-project-icon border-0"
          onPress={() =>
            openProjectMutation.mutateAsync([
              item.id,
              { executeAsync: false, cognitoCredentials: session },
            ])
          }
        />
      )
    case backendModule.ProjectState.openInProgress:
    case backendModule.ProjectState.scheduled:
    case backendModule.ProjectState.provisioned:
    case backendModule.ProjectState.placeholder:
      return (
        <div className="relative">
          <ariaComponents.Button
            size="custom"
            variant="custom"
            noExtraClickZone
            isDisabled={isOtherUserUsingProject}
            icon={StopIcon}
            aria-label={getText('stopExecution')}
            tooltipPlacement="left"
            {...(isOtherUserUsingProject ? { title: getText('otherUserIsUsingProjectError') } : {})}
            className={tailwindMerge.twMerge(
              'size-project-icon border-0 selectable enabled:active',
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
      )
    case backendModule.ProjectState.opened:
      return (
        <div className="flex flex-row gap-0.5">
          <div className="relative">
            <ariaComponents.Button
              size="custom"
              variant="custom"
              noExtraClickZone
              isDisabled={isOtherUserUsingProject}
              icon={StopIcon}
              aria-label={getText('stopExecution')}
              tooltipPlacement="left"
              {...(isOtherUserUsingProject
                ? { title: getText('otherUserIsUsingProjectError') }
                : {})}
              className={tailwindMerge.twMerge(
                'size-project-icon border-0 selectable enabled:active',
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
              variant="custom"
              noExtraClickZone
              className="size-project-icon border-0"
              icon={ArrowUpIcon}
              aria-label={getText('openInEditor')}
              tooltipPlacement="right"
              onPress={() => {
                doOpenEditor(true)
              }}
            />
          )}
        </div>
      )
  }
}
