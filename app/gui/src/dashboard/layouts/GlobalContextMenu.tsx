/** @file A context menu available everywhere in the directory. */
import * as React from 'react'

import { useStore } from 'zustand'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetListEventType from '#/events/AssetListEventType'

import * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'

import * as aria from '#/components/aria'
import ContextMenu from '#/components/ContextMenu'
import ContextMenuEntry from '#/components/ContextMenuEntry'

import UpsertDatalinkModal from '#/modals/UpsertDatalinkModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'

import { useDriveStore } from '#/providers/DriveProvider'
import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'

/** Props for a {@link GlobalContextMenu}. */
export interface GlobalContextMenuProps {
  readonly hidden?: boolean
  readonly backend: Backend
  readonly rootDirectoryId: backendModule.DirectoryId
  readonly directoryKey: backendModule.DirectoryId | null
  readonly directoryId: backendModule.DirectoryId | null
  readonly doPaste: (
    newParentKey: backendModule.DirectoryId,
    newParentId: backendModule.DirectoryId,
  ) => void
}

/** A context menu available everywhere in the directory. */
export default function GlobalContextMenu(props: GlobalContextMenuProps) {
  const { hidden = false, backend, directoryKey, directoryId, rootDirectoryId } = props
  const { doPaste } = props
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const dispatchAssetListEvent = eventListProvider.useDispatchAssetListEvent()
  const filesInputRef = React.useRef<HTMLInputElement>(null)
  const isCloud = backend.type === backendModule.BackendType.remote
  const driveStore = useDriveStore()
  const hasPasteData = useStore(driveStore, (storeState) => storeState.pasteData != null)

  return (
    <ContextMenu aria-label={getText('globalContextMenuLabel')} hidden={hidden}>
      {!hidden && (
        <aria.Input
          ref={filesInputRef}
          multiple
          type="file"
          id="context_menu_file_input"
          className="hidden"
          onInput={(event) => {
            if (event.currentTarget.files != null) {
              dispatchAssetListEvent({
                type: AssetListEventType.uploadFiles,
                parentKey: directoryKey ?? rootDirectoryId,
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
        action="uploadFiles"
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
                  parentKey: directoryKey ?? rootDirectoryId,
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
            parentKey: directoryKey ?? rootDirectoryId,
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
            parentKey: directoryKey ?? rootDirectoryId,
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
                    parentKey: directoryKey ?? rootDirectoryId,
                    parentId: directoryId ?? rootDirectoryId,
                    name,
                    value,
                  })
                }}
              />,
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
                  dispatchAssetListEvent({
                    type: AssetListEventType.newDatalink,
                    parentKey: directoryKey ?? rootDirectoryId,
                    parentId: directoryId ?? rootDirectoryId,
                    name,
                    value,
                  })
                }}
              />,
            )
          }}
        />
      )}
      {isCloud && directoryKey == null && hasPasteData && (
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
