/** @file Displays information describing a specific version of an asset. */
import LogsIcon from 'enso-assets/logs.svg'

import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import Button from '#/components/styled/Button'

import LogsModal from '#/modals/LogsModal'

import type * as backendModule from '#/services/Backend'

import * as dateTime from '#/utilities/dateTime'

// ===========================
// === AssetProjectSession ===
// ===========================

/** Props for a {@link AssetProjectSession}. */
export interface AssetProjectSessionProps {
  readonly project: backendModule.ProjectAsset
  readonly projectSession: backendModule.ProjectSession
}

/** Displays information describing a specific version of an asset. */
export default function AssetProjectSession(props: AssetProjectSessionProps) {
  const { project, projectSession } = props
  const { backend } = backendProvider.useBackend()
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()

  return (
    <div className="flex w-full flex-1 shrink-0 select-none flex-row gap-4 rounded-2xl p-2">
      <div className="flex flex-1 flex-col">
        <time className="text-not-selected text-xs">
          {getText('onDateX', dateTime.formatDateTime(new Date(projectSession.createdAt)))}
        </time>
      </div>
      <div className="flex items-center gap-1">
        <Button
          active
          image={LogsIcon}
          alt={getText('showLogs')}
          onPress={() => {
            setModal(
              <LogsModal
                logs={backend
                  .getProjectSessionLogs(projectSession.projectSessionId, project.title)
                  .then(logs => logs.join('\n'))}
              />
            )
          }}
        />
      </div>
    </div>
  )
}
