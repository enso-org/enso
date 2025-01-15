/** @file The tab bar for the dashboard page. */
import DriveIcon from '#/assets/drive.svg'
import NetworkIcon from '#/assets/network.svg'
import SettingsIcon from '#/assets/settings.svg'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import TabBar from '#/layouts/TabBar'

import {
  TabType,
  useLaunchedProjects,
  usePage,
  useSetPage,
  type LaunchedProject,
} from '#/providers/ProjectsProvider'
import { useText } from '#/providers/TextProvider'
import type { ProjectId } from '#/services/Backend'
import type { TextId } from 'enso-common/src/text'

/** The props for the {@link DashboardTabBar} component. */
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
    setPage(TabType.drive)
  })

  const tabs = [
    {
      id: TabType.drive,
      icon: DriveIcon,
      'data-testid': 'drive-tab-button',
      labelId: 'drivePageName' satisfies TextId,
      isActive: page === TabType.drive,
      children: getText('drivePageName'),
      // eslint-disable-next-line @typescript-eslint/naming-convention
      Component: TabBar.Tab,
    },
    ...launchedProjects.map(
      (project) =>
        ({
          id: project.id,
          icon: NetworkIcon,
          'data-testid': 'editor-tab-button',
          labelId: 'editorPageName' satisfies TextId,
          // There is no shared enum type, but the other union member is the same type.
          // eslint-disable-next-line @typescript-eslint/no-unsafe-enum-comparison
          isActive: page === project.id,
          children: project.title,
          // eslint-disable-next-line @typescript-eslint/naming-convention
          Component: TabBar.ProjectTab,
          project,
          onClose,
          onLoadEnd,
        }) as const,
    ),
    {
      id: TabType.settings,
      icon: SettingsIcon,
      labelId: 'settingsPageName' satisfies TextId,
      'data-testid': 'settings-tab-button',
      isHidden: page !== TabType.settings,
      children: getText('settingsPageName'),
      onClose: onCloseSettings,
      // eslint-disable-next-line @typescript-eslint/naming-convention
      Component: TabBar.Tab,
    },
  ]

  return (
    <TabBar className="bg-primary/10" items={tabs}>
      {/* @ts-expect-error - Making ts happy here requires too much attention */}
      {(tab) => <tab.Component {...tab} />}
    </TabBar>
  )
}
