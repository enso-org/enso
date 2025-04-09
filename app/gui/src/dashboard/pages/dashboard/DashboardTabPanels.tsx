/** @file The tab panels for the dashboard page. */

import type * as aria from '#/components/aria'

import { Activity } from '#/components/Activity'
import { TabPanel, type TabPanelRenderProps } from '#/components/aria'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { Suspense } from '#/components/Suspense'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useOpenProjectMutation, useRenameProjectMutation } from '#/hooks/projectHooks'
import type { AssetManagementApi } from '#/layouts/AssetsTable'
import { useLaunchedProjects, usePage } from '#/providers/ProjectsProvider'
import type { ProjectId } from '#/services/Backend'
import { omit } from 'enso-common/src/utilities/data/object'
import { lazy, type ReactNode } from 'react'
import { Collection } from 'react-aria-components'

/** The props for the {@link DashboardTabPanels} component. */
export interface DashboardTabPanelsProps {
  readonly initialProjectName: string | null
  readonly ydocUrl: string | null
  readonly assetManagementApiRef: React.RefObject<AssetManagementApi> | null
}

const LazyDrive = lazy(() => import('#/layouts/Drive'))
const LazyEditor = lazy(() => import('#/layouts/Editor'))
const LazySettings = lazy(() => import('#/layouts/Settings'))

/** The tab panels for the dashboard page. */
export function DashboardTabPanels(props: DashboardTabPanelsProps) {
  const { initialProjectName, ydocUrl, assetManagementApiRef } = props

  const page = usePage()

  const launchedProjects = useLaunchedProjects()
  const openProjectMutation = useOpenProjectMutation()
  const renameProjectMutation = useRenameProjectMutation()

  const onRenameProject = useEventCallback(async (newName: string, projectId: ProjectId) => {
    const project = launchedProjects.find((proj) => proj.id === projectId)

    if (project == null) {
      return
    }

    await renameProjectMutation.mutateAsync({ newName, project })
  })

  const tabPanels = [
    {
      id: 'drive',
      className: 'flex min-h-0 grow [&[data-inert]]:hidden',
      wrapInActivity: true,
      shouldForceMount: true,
      children: (
        <LazyDrive
          assetsManagementApiRef={assetManagementApiRef}
          initialProjectName={initialProjectName}
        />
      ),
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
          isOpeningFailed={openProjectMutation.isError}
          openingError={openProjectMutation.error}
          startProject={openProjectMutation.mutate}
          renameProject={onRenameProject}
        />
      ),
    })),

    {
      id: 'settings',
      wrapInActivity: true,
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
