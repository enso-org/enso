/** @file The tab bar for the dashboard page. */
import DriveIcon from '#/assets/drive.svg'
import NetworkIcon from '#/assets/network.svg'
import SettingsIcon from '#/assets/settings.svg'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import TabBar, { ProjectTab, type ProjectTabProps, type TabProps } from '#/layouts/TabBar'

import {
  useLaunchedProjects,
  usePage,
  useSetPage,
  type LaunchedProject,
} from '#/providers/ProjectsProvider'
import { useText } from '#/providers/TextProvider'
import type { ProjectId } from '#/services/Backend'
import type { TextId } from 'enso-common/src/text'

/** Props for a {@link DashboardTabBar}. */
export interface DashboardTabBarProps {
  readonly onCloseProject: (project: LaunchedProject) => void
  readonly onOpenEditor: (projectId: ProjectId) => void
}

/** The tab bar for the dashboard page. */
export function DashboardTabBar(props: DashboardTabBarProps) {
  const { onCloseProject, onOpenEditor } = props

  const { getText } = useText()
  const page = usePage()
  const setPage = useSetPage()
  const launchedProjects = useLaunchedProjects()

  const onLoadEnd = useEventCallback((project: LaunchedProject) => {
    onOpenEditor(project.id)
  })

  const onClose = useEventCallback((project: LaunchedProject) => {
    onCloseProject(project)
  })

  const onCloseSettings = useEventCallback(() => {
    setPage('drive')
  })

  const tabs: readonly ((ProjectTabProps & { type: 'project' }) | (TabProps & { type: 'tab' }))[] =
    [
      {
        type: 'tab',
        id: 'drive',
        icon: DriveIcon,
        'data-testid': 'drive-tab-button',
        labelId: 'drivePageName' satisfies TextId,
        isActive: page === 'drive',
        children: getText('drivePageName'),
      },
      ...launchedProjects.map(
        (project) =>
          ({
            type: 'project',
            id: project.id,
            icon: NetworkIcon,
            'data-testid': 'editor-tab-button',
            labelId: 'editorPageName' satisfies TextId,
            isActive: page === project.id,
            children: project.title,
            project,
            onClose,
            onLoadEnd,
          }) as const,
      ),
      {
        type: 'tab',
        id: 'settings',
        icon: SettingsIcon,
        labelId: 'settingsPageName' satisfies TextId,
        'data-testid': 'settings-tab-button',
        isActive: true,
        isHidden: page !== 'settings',
        children: getText('settingsPageName'),
        onClose: onCloseSettings,
      },
    ]

  return (
    <TabBar className="bg-primary/10" items={tabs}>
      {(tabProps) => {
        switch (tabProps.type) {
          case 'tab': {
            return <TabBar.Tab {...tabProps} />
          }
          case 'project': {
            return <ProjectTab {...tabProps} />
          }
        }
      }}
    </TabBar>
  )
}
