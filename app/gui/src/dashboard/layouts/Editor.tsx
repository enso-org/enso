/** @file The container that launches the IDE. */
import * as errorBoundary from '#/components/ErrorBoundary'
import * as suspense from '#/components/Suspense'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import * as gtagHooks from '#/hooks/gtagHooks'
import * as projectHooks from '#/hooks/projectHooks'
import { useTimeoutCallback } from '#/hooks/timeoutHooks'
import * as backendProvider from '#/providers/BackendProvider'
import type { LaunchedProject } from '#/providers/ProjectsProvider'
import * as textProvider from '#/providers/TextProvider'
import * as backendModule from '#/services/Backend'
import * as twMerge from '#/utilities/tailwindMerge'
import { vueComponent } from '#/utilities/vue'
import * as reactQuery from '@tanstack/react-query'
import * as React from 'react'
import invariant from 'tiny-invariant'

const ProjectViewTab = React.lazy(() =>
  import('@/ProjectViewTab.vue').then(({ default: vue }) => vueComponent(vue)),
)

/** Props for the GUI editor root component. */
export type ProjectViewTabProps = React.ComponentProps<typeof ProjectViewTab>

/** Props for an {@link Editor}. */
export interface EditorProps {
  readonly isOpeningFailed: boolean
  readonly openingError: Error | null
  readonly startProject: (project: LaunchedProject) => void
  readonly project: LaunchedProject
  readonly hidden: boolean
  readonly ydocUrl: string | null
  readonly renameProject: (newName: string, projectId: backendModule.ProjectId) => void
  readonly projectId: backendModule.ProjectId
}

/** The container that launches the IDE. */
export default function Editor(props: EditorProps) {
  const { project, hidden, startProject, isOpeningFailed, openingError } = props

  const backend = backendProvider.useBackendForProjectType(project.type)
  const remoteBackend = backendProvider.useRemoteBackend()

  const projectStatusQuery = projectHooks.createGetProjectDetailsQuery({
    assetId: project.id,
    backend,
  })

  const queryClient = reactQuery.useQueryClient()

  const isHybrid = project.hybrid != null

  const projectQuery = reactQuery.useSuspenseQuery({
    ...projectStatusQuery,
    select: (data) => {
      const isProjectOpening = projectHooks.OPENING_PROJECT_STATES.has(data.state.type)
      const isProjectClosed = projectHooks.CLOSED_PROJECT_STATES.has(data.state.type)
      const isProjectOpened = projectHooks.OPENED_PROJECT_STATES.has(data.state.type)
      const isProjectClosing = projectHooks.CLOSING_PROJECT_STATES.has(data.state.type)

      return { ...data, isProjectOpening, isProjectClosed, isProjectOpened, isProjectClosing }
    },
  })

  // If it's a hybrid project, we need to fetch the project details from the remote backend.
  const {
    data: { name },
  } = reactQuery.useSuspenseQuery({
    ...projectHooks.createGetProjectDetailsQuery({
      assetId: isHybrid ? project.hybrid.cloudProjectId : project.id,
      backend: isHybrid ? remoteBackend : backend,
    }),
    select: (projectDetails) => ({ name: projectDetails.name }),
  })

  const { isProjectClosed, isProjectOpening, isProjectOpened, isProjectClosing } = projectQuery.data

  React.useEffect(() => {
    if (isProjectClosed) {
      startProject(project)
    }
  }, [isProjectClosed, startProject, project])

  useTimeoutCallback({
    callback: () => {
      const queryState = queryClient.getQueryCache().find({ queryKey: projectStatusQuery.queryKey })

      queryState?.setState({
        error: new Error('Timeout opening the project'),
        status: 'error',
      })
    },
    ms: projectHooks.getTimeoutBasedOnTheBackendType(backend.type),
    deps: [],
    isDisabled: !isProjectOpening || projectQuery.isError,
  })

  if (isOpeningFailed) {
    return (
      <errorBoundary.ErrorDisplay
        error={openingError}
        resetErrorBoundary={() => {
          if (isProjectClosed) {
            startProject(project)
          }
        }}
      />
    )
  }

  return (
    <div
      className={twMerge.twJoin('contents', hidden && 'hidden')}
      data-testvalue={project.id}
      data-testid="editor"
    >
      {(() => {
        // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
        switch (true) {
          case projectQuery.isError:
            return (
              <errorBoundary.ErrorDisplay
                error={projectQuery.error}
                resetErrorBoundary={() => projectQuery.refetch()}
              />
            )

          case isProjectClosed:
          case isProjectClosing:
          case isProjectOpening:
            return <suspense.Loader minHeight="full" />

          case isProjectOpened:
            return (
              <EditorInternal
                {...props}
                openedProject={projectQuery.data}
                backendType={project.type}
                projectName={name}
              />
            )

          default:
            return null
        }
      })()}
    </div>
  )
}

/** Props for an {@link EditorInternal}. */
interface EditorInternalProps extends Omit<EditorProps, 'project'> {
  readonly openedProject: backendModule.Project
  readonly backendType: backendModule.BackendType
  readonly projectName: string
}

/** An internal editor. */
function EditorInternal(props: EditorInternalProps) {
  const { hidden, ydocUrl, renameProject, openedProject, backendType, projectName } = props

  const { getText } = textProvider.useText()
  const gtagEvent = gtagHooks.useGtagEvent()

  const localBackend = backendProvider.useLocalBackend()
  const remoteBackend = backendProvider.useRemoteBackend()

  React.useEffect(() => {
    if (!hidden) {
      return gtagHooks.gtagOpenCloseCallback(gtagEvent, 'open_workflow', 'close_workflow')
    }
  }, [hidden, gtagEvent])

  const onRenameProject = useEventCallback((newName: string) => {
    renameProject(newName, openedProject.projectId)
  })

  const jsonAddress = openedProject.jsonAddress
  const binaryAddress = openedProject.binaryAddress
  const ydocAddress = openedProject.ydocAddress ?? ydocUrl ?? ''
  const projectBackend =
    backendType === backendModule.BackendType.remote ? remoteBackend : localBackend

  invariant(jsonAddress != null, getText('noJSONEndpointError'))
  invariant(binaryAddress != null, getText('noBinaryEndpointError'))

  const appProps = {
    hidden,
    projectViewProps: {
      projectId: openedProject.projectId,
      projectName: openedProject.packageName,
      projectDisplayedName: projectName,
      engine: { rpcUrl: jsonAddress, dataUrl: binaryAddress, ydocUrl: ydocAddress },
      renameProject: onRenameProject,
      projectBackend,
      remoteBackend,
    },
  } as const

  const key: string = appProps.projectViewProps.projectId

  // Currently the GUI component needs to be fully rerendered whenever the project is changed. Once
  // this is no longer necessary, the `key` could be removed.
  return <ProjectViewTab key={key} {...appProps} />
}
