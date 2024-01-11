/** @file Modal for confirming delete of any type of asset. */
import * as React from 'react'

import Modal from '#/components/Modal'

// =================
// === LogsModal ===
// =================

/** Props for a {@link LogsModal}. */
export interface LogsModalProps {
    logs: string
}

/** A modal for confirming the deletion of an asset. */
export default function LogsModal(props: LogsModalProps) {
    const { logs } = props

    return (
        <Modal centered className="bg-dim">
            <div
                tabIndex={-1}
                className="relative flex flex-col gap-2 rounded-2xl pointer-events-auto w-4/5 h-4/5 p-4 before:absolute before:rounded-2xl before:bg-frame-selected before:backdrop-blur-3xl before:inset-0 before:w-full before:h-full"
            >
                <h2 className="relative text-sm font-bold">Logs</h2>
                <pre className="relative overflow-auto">
                    <code>{logs}</code>
                </pre>
            </div>
        </Modal>
    )
}
