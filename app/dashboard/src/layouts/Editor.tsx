/** @file The container that launches the IDE. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as appUtils from '#/appUtils'

import * as gtagHooks from '#/hooks/gtagHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import * as dashboard from '#/pages/dashboard/Dashboard'

import * as errorBoundary from '#/components/ErrorBoundary'
import * as suspense from '#/components/Suspense'

import * as backendModule from '#/services/Backend'

import * as twMerge from '#/utilities/tailwindMerge'

// ====================
// === StringConfig ===
// ====================

/** A configuration in which values may be strings or nested configurations. */
interface StringConfig {
  readonly [key: string]: StringConfig | string
}

// ========================
// === GraphEditorProps ===
// ========================

/** Props for the GUI editor root component. */
export interface GraphEditorProps {
  readonly config: StringConfig | null
  readonly projectId: string
  readonly hidden: boolean
  readonly ignoreParamsRegex?: RegExp
  readonly logEvent: (message: string, projectId?: string | null, metadata?: object | null) => void
  readonly renameProject: (newName: string) => void
}

// =========================
// === GraphEditorRunner ===
// =========================

/** The value passed from the entrypoint to the dashboard, which enables the dashboard to
 * open a new IDE instance. */
export type GraphEditorRunner = React.ComponentType<GraphEditorProps>

// =================
// === Constants ===
// =================

const IGNORE_PARAMS_REGEX = new RegExp(`^${appUtils.SEARCH_PARAMS_PREFIX}(.+)$`)

// ==============
// === Editor ===
// ==============

/** Props for an {@link Editor}. */
export interface EditorProps {
  readonly isOpening: boolean
  readonly startProject: (project: dashboard.Project) => void
  readonly project: dashboard.Project
  readonly hidden: boolean
  readonly ydocUrl: string | null
  readonly appRunner: GraphEditorRunner | null
  readonly renameProject: (newName: string) => void
  readonly projectId: backendModule.ProjectAsset['id']
}

/** The container that launches the IDE. */
export default function Editor(props: EditorProps) {
  const { project, hidden, isOpening, startProject } = props

  const remoteBackend = backendProvider.useRemoteBackendStrict()
  const localBackend = backendProvider.useLocalBackend()

  const projectStatusQuery = dashboard.createGetProjectDetailsQuery({
    type: project.type,
    assetId: project.id,
    parentId: project.parentId,
    title: project.title,
    remoteBackend,
    localBackend,
  })

  const projectQuery = reactQuery.useQuery({
    ...projectStatusQuery,
    networkMode: project.type === backendModule.BackendType.remote ? 'online' : 'always',
  })

  if (!isOpening && projectQuery.data?.state.type === backendModule.ProjectState.closed) {
    startProject(project)
  }

  return (
    <div
      className={twMerge.twJoin('contents', hidden && 'hidden')}
      data-testid="gui-editor-root"
      data-testvalue={project.id}
    >
      {(() => {
        if (projectQuery.isError) {
          return (
            <errorBoundary.ErrorDisplay
              error={projectQuery.error}
              resetErrorBoundary={projectQuery.refetch}
            />
          )
        } else if (
          projectQuery.isLoading ||
          projectQuery.data?.state.type !== backendModule.ProjectState.opened
        ) {
          return <suspense.Loader loaderProps={{ minHeight: 'full' }} />
        } else {
          return (
            <suspense.Suspense>
              <EditorInternal {...props} openedProject={projectQuery.data} />{' '}
            </suspense.Suspense>
          )
        }
      })()}
    </div>
  )
}

// ======================
// === EditorInternal ===
// ======================

/** Props for an {@link EditorInternal}. */
interface EditorInternalProps extends Omit<EditorProps, 'project'> {
  readonly openedProject: backendModule.Project
}

/** An internal editor. */
function EditorInternal(props: EditorInternalProps) {
  const { hidden, ydocUrl, appRunner: AppRunner, renameProject, openedProject } = props

  const { getText } = textProvider.useText()
  const gtagEvent = gtagHooks.useGtagEvent()

  const remoteBackend = backendProvider.useRemoteBackend()

  const logEvent = React.useCallback(
    (message: string, projectId?: string | null, metadata?: object | null) => {
      if (remoteBackend) {
        void remoteBackend.logEvent(message, projectId, metadata)
      }
    },
    [remoteBackend]
  )

  React.useEffect(() => {
    if (hidden) {
      return
    } else {
      return gtagHooks.gtagOpenCloseCallback(gtagEvent, 'open_workflow', 'close_workflow')
    }
  }, [hidden, gtagEvent])

  const appProps = React.useMemo<GraphEditorProps>(() => {
    const jsonAddress = openedProject.jsonAddress
    const binaryAddress = openedProject.binaryAddress
    const ydocAddress = ydocUrl ?? ''

    if (jsonAddress == null) {
      throw new Error(getText('noJSONEndpointError'))
    } else if (binaryAddress == null) {
      throw new Error(getText('noBinaryEndpointError'))
    } else {
      return {
        config: {
          engine: { rpcUrl: jsonAddress, dataUrl: binaryAddress, ydocUrl: ydocAddress },
          startup: { project: openedProject.packageName, displayedProjectName: openedProject.name },
          window: { topBarOffset: '0' },
        },
        projectId: openedProject.projectId,
        hidden,
        ignoreParamsRegex: IGNORE_PARAMS_REGEX,
        logEvent,
        renameProject,
      }
    }
  }, [openedProject, ydocUrl, getText, hidden, logEvent, renameProject])

  // Currently the GUI component needs to be fully rerendered whenever the project is changed. Once
  // this is no longer necessary, the `key` could be removed.
  return AppRunner == null ? null : <AppRunner key={appProps.projectId} {...appProps} />
}
