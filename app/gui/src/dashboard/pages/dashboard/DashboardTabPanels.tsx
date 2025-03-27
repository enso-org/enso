/** @file The tab panels for the dashboard page. */

import * as aria from '#/components/aria'

import { ErrorBoundary } from '#/components/ErrorBoundary'
import { Suspense } from '#/components/Suspense'
import { useLaunchedProjects, usePage } from '#/providers/ProjectsProvider'
import { lazy, type ReactNode } from 'react'
import { Collection } from 'react-aria-components'

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

  const tabPanels = [
    {
      id: 'drive',
      className: 'flex min-h-0 grow [&[data-inert]]:hidden',
      children: <LazyDrive hidden={page !== 'drive'} initialProjectName={initialProjectName} />,
    },

    ...launchedProjects.map((project) => ({
      id: project.id,
      shouldForceMount: true,
      className: 'flex min-h-0 grow [&[data-inert]]:hidden',
      children: <LazyEditor hidden={page !== project.id} ydocUrl={ydocUrl} project={project} />,
    })),

    {
      id: 'settings',
      className: 'flex min-h-0 grow',
      children: <LazySettings />,
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
