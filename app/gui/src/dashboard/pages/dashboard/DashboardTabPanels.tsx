/** @file The tab panels for the dashboard page. */

import type * as aria from '#/components/aria'

import { Activity } from '#/components/Activity'
import { TabPanel, type TabPanelRenderProps } from '#/components/aria'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { Suspense } from '#/components/Suspense'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useRenameProjectMutation } from '#/hooks/projectHooks'
import { useLocalBackend, useRemoteBackend } from '#/providers/BackendProvider'
import { useLaunchedProjects, usePage } from '#/providers/ProjectsProvider'
import { BackendType, type ProjectId } from '#/services/Backend'
import { omit } from 'enso-common/src/utilities/data/object'
import { lazy, type ReactNode } from 'react'
import { Collection } from 'react-aria-components'
import invariant from 'tiny-invariant'

/** The props for the {@link DashboardTabPanels} component. */
export interface DashboardTabPanelsProps {
  readonly initialProjectName: string | null
  readonly ydocUrl: string | null
}

const LazyDrive = lazy(() => import('#/layouts/Drive'))
const LazyEditor = lazy(() => import('#/layouts/Editor'))
const LazySettings = lazy(() => import('#/layouts/Settings'))

/** The tab panels for the dashboard page. */
export function DashboardTabPanels(props: DashboardTabPanelsProps) {
  const { initialProjectName, ydocUrl } = props

  const page = usePage()

  const launchedProjects = useLaunchedProjects()
  const renameProjectMutation = useRenameProjectMutation()
  const remoteBackend = useRemoteBackend()
  const localBackend = useLocalBackend()

  const onRenameProject = useEventCallback(async (newName: string, projectId: ProjectId) => {
    const project = launchedProjects.find((proj) => proj.id === projectId)

    if (project == null) {
      return
    }

    const isHybrid = project.hybrid != null
    const backendType = isHybrid ? BackendType.remote : project.type
    const backend = backendType === BackendType.remote ? remoteBackend : localBackend
    const id = isHybrid ? project.hybrid.cloudProjectId : project.id
    invariant(backend != null, 'Backend is null')

    await renameProjectMutation({
      newName,
      backend,
      project: { ...project, id },
    })
  })

  const tabPanels = [
    {
      id: 'drive',
      className: 'flex min-h-0 grow [&[data-inert]]:hidden',
      wrapInActivity: false,
      shouldForceMount: false,
      children: <LazyDrive initialProjectName={initialProjectName} />,
    },
    ...launchedProjects.map((project) => ({
      id: project.id,
      shouldForceMount: true,
      wrapInActivity: false,
      className: 'flex min-h-0 grow [&[data-inert]]:hidden',
      children: (
        <LazyEditor
          hidden={page !== project.id}
          ydocUrl={ydocUrl}
          project={project}
          projectId={project.id}
          renameProject={onRenameProject}
        />
      ),
    })),
    {
      id: 'settings',
      wrapInActivity: false,
      shouldForceMount: false,
      className: 'flex min-h-0 grow',
      children: <LazySettings />,
    },
  ]

  return (
    <Collection items={tabPanels}>
      {(tabPanelProps: aria.TabPanelProps & { children: ReactNode; wrapInActivity: boolean }) => (
        <TabPanel {...omit(tabPanelProps, 'wrapInActivity')}>
          {({ state }: TabPanelRenderProps) => {
            const content = (
              <Suspense>
                <ErrorBoundary>{tabPanelProps.children}</ErrorBoundary>
              </Suspense>
            )

            // Activity is very experimental and not yet ready for use.
            // We need to figure it out how to hide portals, tooltips and disable keyboard shortcuts.
            if (tabPanelProps.wrapInActivity) {
              return (
                <Activity mode={state.selectedKey === tabPanelProps.id ? 'active' : 'inactive'}>
                  {content}
                </Activity>
              )
            }

            return content
          }}
        </TabPanel>
      )}
    </Collection>
  )
}
