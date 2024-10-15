/** @file A modal for showing logs for a project. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import ReloadIcon from '#/assets/reload.svg'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

import type * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

// ========================
// === ProjectLogsModal ===
// ========================

/** Props for a {@link ProjectLogsModal}. */
export interface ProjectLogsModalProps {
  readonly isOpen: boolean
  readonly backend: Backend
  readonly projectSessionId: backendModule.ProjectSessionId
  readonly projectTitle: string
}

/** A modal for showing logs for a project. */
export default function ProjectLogsModal(props: ProjectLogsModalProps) {
  const { isOpen } = props
  const { getText } = textProvider.useText()

  return (
    <ariaComponents.Dialog title={getText('logs')} type="fullscreen">
      {isOpen && <ProjectLogsModalInternal {...props} />}
    </ariaComponents.Dialog>
  )
}

/** A modal for showing logs for a project. */
function ProjectLogsModalInternal(props: ProjectLogsModalProps) {
  const { backend, projectSessionId, projectTitle } = props
  const { getText } = textProvider.useText()
  const logsQuery = reactQuery.useSuspenseQuery({
    queryKey: ['projectLogs', { projectSessionId, projectTitle }],
    queryFn: async () => {
      const logs = await backend.getProjectSessionLogs(projectSessionId, projectTitle)
      return logs.join('\n')
    },
  })

  return (
    <div className="flex flex-col">
      <div className="flex items-center gap-4 self-start rounded-full border-0.5 border-primary/20 px-[11px] py-2">
        <ariaComponents.Button
          size="medium"
          variant="icon"
          icon={ReloadIcon}
          aria-label={getText('reload')}
          onPress={async () => {
            await logsQuery.refetch()
          }}
        />
      </div>
      <pre className="relative overflow-auto whitespace-pre-wrap">
        <code>{logsQuery.data}</code>
      </pre>
    </div>
  )
}
