/** @file The tab panels for the dashboard page. */

import type * as aria from '#/components/aria'

import { Activity } from '#/components/Activity'
import { TabPanel, type TabPanelRenderProps } from '#/components/aria'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { Suspense } from '#/components/Suspense'
import { useLaunchedProjects } from '#/providers/ProjectsProvider'
import { omit } from 'enso-common/src/utilities/data/object'
import { lazy, type ReactNode } from 'react'
import { Collection } from 'react-aria-components'

/** The props for the {@link DashboardTabPanels} component. */
export interface DashboardTabPanelsProps {
  readonly initialProjectName: string | null
  readonly ydocUrl: string | null
}

const LazyEditorPanel = lazy(() =>
  import('#/layouts/Editor').then((mod) => ({ default: mod.EditorSection })),
)
const LazySettings = lazy(() => import('#/layouts/Settings'))

/** The tab panels for the dashboard page. */
export function DashboardTabPanels(props: DashboardTabPanelsProps) {
  const { ydocUrl } = props

  const launchedProjects = useLaunchedProjects()

  const tabPanels = [
    {
      id: 'drive',
      wrapInActivity: false,
      shouldForceMount: false,
      className: 'flex min-h-0 grow',
      children: <LazyEditorPanel ydocUrl={ydocUrl} />,
    },
    ...launchedProjects.map((project) => ({
      id: project.id,
      wrapInActivity: false,
      shouldForceMount: false,
      className: 'flex min-h-0 grow',
      children: <LazyEditorPanel ydocUrl={ydocUrl} />,
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
