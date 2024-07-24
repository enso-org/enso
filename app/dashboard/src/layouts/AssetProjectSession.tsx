/** @file Displays information describing a specific version of an asset. */
import * as React from 'react'

import LogsIcon from '#/assets/logs.svg'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import Button from '#/components/styled/Button'

import ProjectLogsModal from '#/modals/ProjectLogsModal'

import type * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import * as dateTime from '#/utilities/dateTime'

// ===========================
// === AssetProjectSession ===
// ===========================

/** Props for a {@link AssetProjectSession}. */
export interface AssetProjectSessionProps {
  readonly backend: Backend
  readonly project: backendModule.ProjectAsset
  readonly projectSession: backendModule.ProjectSession
}

/** Displays information describing a specific version of an asset. */
export default function AssetProjectSession(props: AssetProjectSessionProps) {
  const { backend, project, projectSession } = props
  const { getText } = textProvider.useText()
  const [isOpen, setIsOpen] = React.useState(false)

  return (
    <div className="flex w-full flex-1 shrink-0 select-none flex-row gap-4 rounded-2xl p-2">
      <div className="flex flex-1 flex-col">
        <time className="text-xs">
          {dateTime.formatDateTime(new Date(projectSession.createdAt))}
        </time>
      </div>
      <div className="flex items-center gap-1">
        <ariaComponents.DialogTrigger isOpen={isOpen} onOpenChange={setIsOpen}>
          <Button active image={LogsIcon} alt={getText('showLogs')} onPress={() => {}} />

          <ProjectLogsModal
            isOpen={isOpen}
            backend={backend}
            projectSessionId={projectSession.projectSessionId}
            projectTitle={project.title}
          />
        </ariaComponents.DialogTrigger>
      </div>
    </div>
  )
}
