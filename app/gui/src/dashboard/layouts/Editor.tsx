/** @file The container that launches the IDE. */
import { Button } from '#/components/Button'
import { ErrorBoundary, ErrorDisplay } from '#/components/ErrorBoundary'
import { Result } from '#/components/Result'
import { Loader } from '#/components/Suspense'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import * as projectHooks from '#/hooks/projectHooks'
import { useTimeoutCallback } from '#/hooks/timeoutHooks'
import * as backendModule from '#/services/Backend'
import { vueComponent } from '#/utilities/vue'
import type { LaunchedProject } from '$/providers/container'
import { useBackends, useConfig, useText } from '$/providers/react'
import { useVueValue } from '$/providers/react/common'
import * as analytics from '$/utils/analytics'
import ProjectViewTabVue from '@/ProjectViewTab.vue'
import * as reactQuery from '@tanstack/react-query'
import * as React from 'react'
import invariant from 'tiny-invariant'

// eslint-disable-next-line no-restricted-syntax
const ProjectViewTab = vueComponent(ProjectViewTabVue).default

/** Props for the GUI editor root component. */
export type ProjectViewTabProps = React.ComponentProps<typeof ProjectViewTab>

/** Props for an {@link Editor}. */
export interface EditorProps {
  readonly project: LaunchedProject
  readonly hidden?: boolean
  readonly onReadyUpdate?: (value: boolean) => void
  readonly onNameUpdate?: (value: string) => void
}

/** The container that launches the IDE. */
export default function Editor(props: EditorProps) {
  return (
    <ErrorBoundary>
      <EditorContents {...props} />
    </ErrorBoundary>
  )
}

/** The container that launches the IDE. */
function EditorContents(props: EditorProps) {
  const { project, onReadyUpdate, onNameUpdate } = props
  const preventAutoReopen =
    project.type !== backendModule.BackendType.local || project.hybrid != null
  const { getText } = useText()
  const openProjectMutation = projectHooks.useOpenProjectMutation()
  const renameProjectMutation = projectHooks.useRenameProjectMutation()
  const startProject = projectHooks.useReopenProject(openProjectMutation)

  const { localBackend, remoteBackend, backendForType: backendForProjectType } = useBackends()
  const backend = backendForProjectType(project.type)

  const projectStatusQuery = projectHooks.createGetProjectDetailsQuery({
    assetId: project.id,
    backend,
  })

  const queryClient = reactQuery.useQueryClient()

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

  const isHybrid = project.hybrid != null
  // If it's a hybrid project, we need to fetch the project details from the remote backend.
  const {
    data: { name, isHybridOpened },
  } = reactQuery.useSuspenseQuery({
    ...projectHooks.createGetProjectDetailsQuery({
      assetId: isHybrid ? project.hybrid.cloudProjectId : project.id,
      backend: isHybrid ? remoteBackend : backend,
    }),
    select: (projectDetails) => {
      return {
        name: projectDetails.name,
        isHybridOpened:
          isHybrid && projectHooks.OPENED_PROJECT_STATES.has(projectDetails.state.type),
      }
    },
  })

  const { isProjectClosed, isProjectOpening, isProjectOpened, isProjectClosing } = projectQuery.data

  const stableOnReadyUpdate = useEventCallback((value: boolean) => onReadyUpdate?.(value))
  const stableOnNameUpdate = useEventCallback((value: string) => onNameUpdate?.(value))

  const onRenameProject = useEventCallback(async (newName: string) => {
    const backendType = isHybrid ? backendModule.BackendType.remote : project.type
    const backendForRenaming =
      backendType === backendModule.BackendType.remote ? remoteBackend : localBackend
    const id = isHybrid ? project.hybrid.cloudProjectId : project.id
    invariant(backendForRenaming != null, 'Backend is null')

    await renameProjectMutation({
      newName,
      backend: backendForRenaming,
      project: { ...project, id },
    })
  })

  React.useEffect(() => {
    if (
      // Open project unless it is not supposed to be reopened.
      (isProjectClosed && !preventAutoReopen) ||
      // Open hybrid project if it is still marked as opened.
      isHybridOpened
    ) {
      void startProject({ ...project, suppressHybridProjectOpen: isHybridOpened })
    }
  }, [isProjectClosed, startProject, project, preventAutoReopen, isHybridOpened])

  React.useEffect(() => {
    stableOnNameUpdate(name)
  }, [stableOnNameUpdate, name])

  React.useEffect(() => {
    stableOnReadyUpdate(isProjectOpened)
  }, [stableOnReadyUpdate, isProjectOpened])

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

  if (isProjectClosed && preventAutoReopen) {
    return (
      <Result
        status="info"
        title={getText('projectStopped')}
        subtitle={getText('projectStoppedDescription')}
      >
        <Button
          isLoading={isProjectOpening}
          className="mx-auto"
          onPress={async () => {
            await startProject(project)
          }}
        >
          {getText('openProject')}
        </Button>
      </Result>
    )
  }

  if (openProjectMutation.isError) {
    return (
      <ErrorDisplay
        error={openProjectMutation.error}
        resetErrorBoundary={async () => {
          if (isProjectClosed) {
            await startProject(project)
          }
        }}
      />
    )
  }

  return (
    <div className="contents" data-testvalue={project.id} data-testid="editor">
      {(() => {
        // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
        switch (true) {
          case projectQuery.isError:
            return (
              <ErrorDisplay
                error={projectQuery.error}
                resetErrorBoundary={() => projectQuery.refetch()}
              />
            )

          case isProjectClosed:
          case isProjectClosing:
          case isProjectOpening:
            return <Loader minHeight="full" />

          case isProjectOpened:
            return (
              <EditorInternal
                {...props}
                openedProject={projectQuery.data}
                backendType={project.type}
                renameProject={onRenameProject}
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
  readonly renameProject: (newName: string) => void
  readonly projectName: string
}

/** An internal editor. */
function EditorInternal(props: EditorInternalProps) {
  const { hidden = false, renameProject, openedProject, backendType, projectName } = props

  const { getText } = useText()
  const config = useConfig()
  const ydocUrl = useVueValue(React.useCallback(() => config.ydocUrl, [config]))

  const { localBackend, remoteBackend } = useBackends()

  React.useEffect(() => {
    if (!hidden) {
      return analytics.editorOpenCloseCallback()
    }
  }, [hidden])

  const onRenameProject = useEventCallback((newName: string) => {
    renameProject(newName)
  })

  const jsonAddress = openedProject.jsonAddress
  const binaryAddress = openedProject.binaryAddress
  const ydocAddress = openedProject.ydocAddress ?? ydocUrl ?? ''
  const projectBackend =
    backendType === backendModule.BackendType.remote ? remoteBackend : localBackend

  invariant(jsonAddress != null, getText('noJSONEndpointError'))
  invariant(binaryAddress != null, getText('noBinaryEndpointError'))

  const appProps: ProjectViewTabProps = {
    hidden,
    projectViewProps: {
      projectId: openedProject.projectId,
      projectInitialName: openedProject.packageName,
      projectDisplayedName: projectName,
      projectPath: openedProject.ensoPath,
      engine: { rpcUrl: jsonAddress, dataUrl: binaryAddress, ydocUrl: ydocAddress },
      renameProject: onRenameProject,
      projectBackend,
      remoteBackend,
    },
  }

  // Currently the GUI component needs to be fully rerendered whenever the project is changed. Once
  // this is no longer necessary, the `key` could be removed.
  return <ProjectViewTab key={openedProject.projectId} {...appProps} />
}
