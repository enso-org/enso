/** @file A context menu available everywhere in the directory. */
import * as React from 'react'

import * as backendHooks from '#/hooks/backendHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import ContextMenu from '#/components/ContextMenu'
import ContextMenuEntry from '#/components/ContextMenuEntry'

import UpsertDatalinkModal from '#/modals/UpsertDatalinkModal'
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
  readonly doPaste: (
    newParentKey: backendModule.DirectoryId,
    newParentId: backendModule.DirectoryId
  ) => void
}

/** A context menu available everywhere in the directory. */
export default function GlobalContextMenu(props: GlobalContextMenuProps) {
  const { hidden = false, backend, hasPasteData, directoryId } = props
  const { rootDirectoryId } = props
  const { doPaste } = props
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const filesInputRef = React.useRef<HTMLInputElement>(null)
  const isCloud = backend.type === backendModule.BackendType.remote

  const createDatalinkMutation = backendHooks.useBackendMutation(backend, 'createDatalink')

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
              dispatchAssetListEvent({
                type: AssetListEventType.uploadFiles,
                parentId: directoryId ?? rootDirectoryId,
                files: Array.from(event.currentTarget.files),
              })
              unsetModal()
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
                dispatchAssetListEvent({
                  type: AssetListEventType.uploadFiles,
                  parentId: directoryId ?? rootDirectoryId,
                  files: Array.from(input.files),
                })
                unsetModal()
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
          unsetModal()
          dispatchAssetListEvent({
            type: AssetListEventType.newProject,
            parentId: directoryId ?? rootDirectoryId,
            templateId: null,
            datalinkId: null,
            preferredName: null,
          })
        }}
      />
      <ContextMenuEntry
        hidden={hidden}
        action="newFolder"
        doAction={() => {
          unsetModal()
          dispatchAssetListEvent({
            type: AssetListEventType.newFolder,
            parentId: directoryId ?? rootDirectoryId,
          })
        }}
      />
      {isCloud && (
        <ContextMenuEntry
          hidden={hidden}
          action="newSecret"
          doAction={() => {
            setModal(
              <UpsertSecretModal
                id={null}
                name={null}
                doCreate={(name, value) => {
                  dispatchAssetListEvent({
                    type: AssetListEventType.newSecret,
                    parentId: directoryId ?? rootDirectoryId,
                    name,
                    value,
                  })
                }}
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
              <UpsertDatalinkModal
                doCreate={(name, value) => {
                  createDatalinkMutation.mutate([
                    {
                      type: AssetListEventType.newDatalink,
                      parentId: directoryId ?? rootDirectoryId,
                      name,
                      value,
                    },
                  ])
                }}
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
            unsetModal()
            doPaste(rootDirectoryId, rootDirectoryId)
          }}
        />
      )}
    </ContextMenu>
  )
}
