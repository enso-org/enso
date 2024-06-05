/** @file A context menu available everywhere in the directory. */
import * as React from 'react'

import * as backendHooks from '#/hooks/backendHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import ContextMenu from '#/components/ContextMenu'
import ContextMenuEntry from '#/components/ContextMenuEntry'

import CreateDatalinkModal from '#/modals/CreateDatalinkModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

/** Props for a {@link GlobalContextMenu}. */
export interface GlobalContextMenuProps {
  readonly hidden?: boolean
  readonly backend: Backend
  readonly hasPasteData: boolean
  readonly rootDirectoryId: backendModule.DirectoryId
  readonly directoryId: backendModule.DirectoryId | null
  readonly doPaste: (newParentId: backendModule.DirectoryId) => void
}

/** A context menu available everywhere in the directory. */
export default function GlobalContextMenu(props: GlobalContextMenuProps) {
  const { hidden = false, backend, hasPasteData, directoryId } = props
  const { rootDirectoryId } = props
  const { doPaste } = props
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const filesInputRef = React.useRef<HTMLInputElement>(null)
  const isCloud = backend.type === backendModule.BackendType.remote

  const uploadFilesMutation = backendHooks.useBackendUploadFilesMutation(backend)
  const createDirectoryMutation = backendHooks.useBackendCreateDirectoryMutation(backend)
  const createProjectMutation = backendHooks.useBackendCreateProjectMutation(backend)

  return (
    <ContextMenu aria-label={getText('globalContextMenuLabel')} hidden={hidden}>
      {!hidden && (
        <aria.Input
          ref={filesInputRef}
          multiple
          type="file"
          id="context_menu_file_input"
          className="hidden"
          onInput={event => {
            if (event.currentTarget.files != null) {
              uploadFilesMutation.mutate([
                event.currentTarget.files,
                directoryId ?? rootDirectoryId,
              ])
            }
          }}
        />
      )}
      <ContextMenuEntry
        hidden={hidden}
        action={'uploadFiles'}
        doAction={() => {
          if (filesInputRef.current?.isConnected === true) {
            filesInputRef.current.click()
          } else {
            const input = document.createElement('input')
            input.type = 'file'
            input.style.display = 'none'
            document.body.appendChild(input)
            input.addEventListener('input', () => {
              if (input.files != null) {
                uploadFilesMutation.mutate([input.files, directoryId ?? rootDirectoryId])
              }
            })
            input.click()
            input.remove()
          }
        }}
      />
      <ContextMenuEntry
        hidden={hidden}
        action="newProject"
        doAction={() => {
          createProjectMutation.mutate([
            { projectName: 'New Project', parentDirectoryId: directoryId ?? rootDirectoryId },
          ])
        }}
      />
      <ContextMenuEntry
        hidden={hidden}
        action="newFolder"
        doAction={() => {
          createDirectoryMutation.mutate([
            { title: 'New Folder', parentId: directoryId ?? rootDirectoryId },
          ])
        }}
      />
      {isCloud && (
        <ContextMenuEntry
          hidden={hidden}
          action="newSecret"
          doAction={() => {
            setModal(
              <UpsertSecretModal
                backend={backend}
                asset={null}
                parentDirectoryId={rootDirectoryId}
              />
            )
          }}
        />
      )}
      {isCloud && (
        <ContextMenuEntry
          hidden={hidden}
          action="newDatalink"
          doAction={() => {
            setModal(
              <CreateDatalinkModal
                backend={backend}
                parentDirectoryId={directoryId ?? rootDirectoryId}
              />
            )
          }}
        />
      )}
      {isCloud && hasPasteData && (
        <ContextMenuEntry
          hidden={hidden}
          action="paste"
          doAction={() => {
            doPaste(rootDirectoryId)
          }}
        />
      )}
    </ContextMenu>
  )
}
