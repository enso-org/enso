/** @file A modal for showing logs for a project. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

import type * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

// ========================
// === ProjectLogsModal ===
// ========================

/** Props for a {@link ProjectLogsModal}. */
export interface ProjectLogsModalProps {
  readonly backend: Backend
  readonly projectSessionId: backendModule.ProjectSessionId
  readonly projectTitle: string
}

/** A modal for showing logs for a project. */
export default function ProjectLogsModal(props: ProjectLogsModalProps) {
  const { backend, projectSessionId, projectTitle } = props
  const { getText } = textProvider.useText()
  const [isOpen, setIsOpen] = React.useState(false)
  const logsQuery = reactQuery.useQuery({
    enabled: isOpen,
    queryKey: ['projectLogs', { projectSessionId, projectTitle }],
    queryFn: async () => {
      const logs = await backend.getProjectSessionLogs(projectSessionId, projectTitle)
      return logs.join('\n')
    },
  })

  return (
    <ariaComponents.Dialog onOpenChange={setIsOpen} title={getText('logs')} type="fullscreen">
      <pre className="relative overflow-auto whitespace-pre-wrap">
        <code>{logsQuery.data ?? ''}</code>
      </pre>
    </ariaComponents.Dialog>
  )
}
