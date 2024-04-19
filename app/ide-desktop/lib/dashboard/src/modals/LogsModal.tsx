/** @file A modal for showing logs for a project. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import Modal from '#/components/Modal'

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
  const [logs, setLogs] = React.useState(() => (logsRaw instanceof Promise ? null : logsRaw))
  const { getText } = textProvider.useText()

  React.useEffect(() => {
    if (logsRaw instanceof Promise) {
      void logsRaw.then(newLogs => {
        setLogs(newLogs)
      })
    }
  }, [logsRaw])

  return (
    <Modal centered className="bg-dim">
      <div
        tabIndex={-1}
        className="before:bg-frame-selected pointer-events-auto relative flex h-4/5 w-4/5 flex-col gap-2 rounded-2xl p-4 before:absolute before:inset-0 before:h-full before:w-full before:rounded-2xl before:backdrop-blur-3xl"
      >
        <aria.Heading level={2} className="relative text-sm font-bold">
          {getText('logs')}
        </aria.Heading>
        <pre className="relative overflow-auto">
          <code>{logs}</code>
        </pre>
      </div>
    </Modal>
  )
}
