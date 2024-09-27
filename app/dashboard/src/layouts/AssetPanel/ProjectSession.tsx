/** @file Displays information describing a specific version of an asset. */
import { useState } from 'react'

import LogsIcon from '#/assets/logs.svg'
import { Button, DialogTrigger } from '#/components/AriaComponents'
import ProjectLogsModal from '#/modals/ProjectLogsModal'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import type { ProjectSession as BackendProjectSession, ProjectAsset } from '#/services/Backend'
import { formatDateTime } from '#/utilities/dateTime'

/** Props for a {@link ProjectSession}. */
export interface ProjectSessionProps {
  readonly backend: Backend
  readonly project: ProjectAsset
  readonly projectSession: BackendProjectSession
}

/** Displays information describing a specific version of an asset. */
export default function ProjectSession(props: ProjectSessionProps) {
  const { backend, project, projectSession } = props
  const { getText } = useText()
  const [isOpen, setIsOpen] = useState(false)

  return (
    <div className="flex w-full flex-1 shrink-0 select-none flex-row gap-4 rounded-2xl p-2">
      <div className="flex flex-1 flex-col">
        <time className="text-xs">{formatDateTime(new Date(projectSession.createdAt))}</time>
      </div>
      <div className="flex items-center gap-1">
        <DialogTrigger isOpen={isOpen} onOpenChange={setIsOpen}>
          <Button isActive icon={LogsIcon} aria-label={getText('showLogs')} />

          <ProjectLogsModal
            isOpen={isOpen}
            backend={backend}
            projectSessionId={projectSession.projectSessionId}
            projectTitle={project.title}
          />
        </DialogTrigger>
      </div>
    </div>
  )
}
