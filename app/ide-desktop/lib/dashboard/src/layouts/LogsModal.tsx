/** @file A modal for showing logs for a project. */
import * as React from 'react'

import Modal from '#/components/Modal'

// =================
// === LogsModal ===
// =================

/** Props for a {@link LogsModal}. */
export interface LogsModalProps {
  readonly logs: string
}

/** A modal for showing logs for a project. */
export default function LogsModal(props: LogsModalProps) {
  const { logs } = props

  return (
    <Modal centered className="bg-dim">
      <div
        tabIndex={-1}
        className="before:bg-frame-selected pointer-events-auto relative flex h-4/5 w-4/5 flex-col gap-2 rounded-2xl p-4 before:absolute before:inset-0 before:h-full before:w-full before:rounded-2xl before:backdrop-blur-3xl"
      >
        <h2 className="relative text-sm font-bold">Logs</h2>
        <pre className="relative overflow-auto">
          <code>{logs}</code>
        </pre>
      </div>
    </Modal>
  )
}
