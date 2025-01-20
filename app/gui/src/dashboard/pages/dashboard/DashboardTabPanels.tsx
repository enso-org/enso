/** @file The tab panels for the dashboard page. */

import * as aria from '#/components/aria'

import { ErrorBoundary } from '#/components/ErrorBoundary'
import { Suspense } from '#/components/Suspense'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useOpenProjectMutation, useRenameProjectMutation } from '#/hooks/projectHooks'
import type { AssetManagementApi } from '#/layouts/AssetsTable'
import Drive from '#/layouts/Drive'
import Editor from '#/layouts/Editor'
import Settings from '#/layouts/Settings'
import { TabType, useLaunchedProjects, usePage } from '#/providers/ProjectsProvider'
import type { ProjectId } from '#/services/Backend'
import type { ReactNode } from 'react'
import { Collection } from 'react-aria-components'

/** The props for the {@link DashboardTabPanels} component. */
export interface DashboardTabPanelsProps {
  readonly initialProjectName: string | null
  readonly ydocUrl: string | null
  readonly assetManagementApiRef: React.RefObject<AssetManagementApi> | null
}

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
      id: TabType.drive,
      className: 'flex min-h-0 grow [&[data-inert]]:hidden',
      children: (
        <Drive
          assetsManagementApiRef={assetManagementApiRef}
          hidden={page !== TabType.drive}
          initialProjectName={initialProjectName}
        />
      ),
    },

    ...launchedProjects.map((project) => ({
      id: project.id,
      shouldForceMount: true,
      className: 'flex min-h-0 grow [&[data-inert]]:hidden',
      children: (
        <Editor
          // There is no shared enum type, but the other union member is the same type.
          // eslint-disable-next-line @typescript-eslint/no-unsafe-enum-comparison
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
      id: TabType.settings,
      className: 'flex min-h-0 grow',
      children: <Settings />,
    },
  ]

  return (
    <Collection items={tabPanels}>
      {(tabPanelProps: aria.TabPanelProps & { children: ReactNode }) => (
        <aria.TabPanel {...tabPanelProps}>
          <Suspense>
            <ErrorBoundary>{tabPanelProps.children}</ErrorBoundary>
          </Suspense>
        </aria.TabPanel>
      )}
    </Collection>
  )
}
