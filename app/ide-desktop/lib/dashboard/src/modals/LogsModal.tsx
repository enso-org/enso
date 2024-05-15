/** @file A modal for showing logs for a project. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

import * as uniqueString from '#/utilities/uniqueString'

// =================
// === LogsModal ===
// =================

/** Props for a {@link LogsModal}. */
export interface LogsModalProps {
  readonly logs: Promise<string> | string
}

/** A modal for showing logs for a project. */
export default function LogsModal(props: LogsModalProps) {
  const { logs: logsRaw } = props
  const { getText } = textProvider.useText()
  const logsQuery = reactQuery.useQuery({
    queryKey: ['logs', uniqueString.uniqueString()],
    queryFn: () => logsRaw,
  })

  return (
    <ariaComponents.Dialog type="fullscreen" className="bg-dim">
      <div
        tabIndex={-1}
        className="pointer-events-auto relative flex h-4/5 w-4/5 flex-col gap-4 overflow-auto rounded-default p-4 before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-3xl"
        onClick={event => {
          event.stopPropagation()
        }}
      >
        <aria.Heading level={2} className="relative text-lg font-bold">
          {getText('logs')}
        </aria.Heading>
        <pre className="relative overflow-auto whitespace-pre-wrap">
          <code>{logsQuery.data ?? ''}</code>
        </pre>
      </div>
    </ariaComponents.Dialog>
  )
}
