/** @file A context menu available everywhere in the directory. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'

import type * as assetListEventModule from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'

import ContextMenu from '#/components/ContextMenu'
import MenuEntry from '#/components/MenuEntry'

import UpsertDataLinkModal from '#/modals/UpsertDataLinkModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'

import type * as backendModule from '#/services/Backend'

/** Props for a {@link GlobalContextMenu}. */
export interface GlobalContextMenuProps {
  readonly hidden?: boolean
  readonly isCloud: boolean
  readonly hasPasteData: boolean
  readonly directoryKey: backendModule.DirectoryId | null
  readonly directory: backendModule.SmartDirectory | null
  readonly dispatchAssetListEvent: (event: assetListEventModule.AssetListEvent) => void
  readonly doPaste: (newParentKey: backendModule.AssetId) => void
}

/** A context menu available everywhere in the directory. */
export default function GlobalContextMenu(props: GlobalContextMenuProps) {
  const { hidden = false, isCloud, hasPasteData, directoryKey, directory } = props
  const { dispatchAssetListEvent, doPaste } = props
  const { user } = authProvider.useNonPartialUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const rootDirectory = React.useMemo(() => user?.rootDirectory(), [user])
  const filesInputRef = React.useRef<HTMLInputElement>(null)
  return rootDirectory == null ? (
    <></>
  ) : (
    <ContextMenu hidden={hidden}>
      {!hidden && (
        <input
          ref={filesInputRef}
          multiple
          type="file"
          id="context_menu_file_input"
          {...(isCloud ? {} : { accept: '.enso-project' })}
          className="hidden"
          onInput={event => {
            if (event.currentTarget.files != null) {
              dispatchAssetListEvent({
                type: AssetListEventType.uploadFiles,
                parentKey: directoryKey ?? rootDirectory.value.id,
                parent: directory ?? rootDirectory,
                files: Array.from(event.currentTarget.files),
              })
              unsetModal()
            }
          }}
        />
      )}
      <MenuEntry
        hidden={hidden}
        action={isCloud ? 'uploadProjects' : 'uploadFiles'}
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
                  parentKey: directoryKey ?? rootDirectory.value.id,
                  parent: directory ?? rootDirectory,
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
      {isCloud && (
        <MenuEntry
          hidden={hidden}
          action="newProject"
          doAction={() => {
            unsetModal()
            dispatchAssetListEvent({
              type: AssetListEventType.newProject,
              parentKey: directoryKey ?? rootDirectory.value.id,
              parent: directory ?? rootDirectory,
              templateId: null,
              templateName: null,
              onSpinnerStateChange: null,
            })
          }}
        />
      )}
      {isCloud && (
        <MenuEntry
          hidden={hidden}
          action="newFolder"
          doAction={() => {
            unsetModal()
            dispatchAssetListEvent({
              type: AssetListEventType.newFolder,
              parentKey: directoryKey ?? rootDirectory.value.id,
              parent: directory ?? rootDirectory,
            })
          }}
        />
      )}
      {isCloud && (
        <MenuEntry
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
                    parentKey: directoryKey ?? rootDirectory.value.id,
                    parent: directory ?? rootDirectory,
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
        <MenuEntry
          hidden={hidden}
          action="newDataLink"
          doAction={() => {
            setModal(
              <UpsertDataLinkModal
                doCreate={(name, value) => {
                  dispatchAssetListEvent({
                    type: AssetListEventType.newDataLink,
                    parentKey: directoryKey ?? rootDirectory.value.id,
                    parent: directory ?? rootDirectory,
                    name,
                    value,
                  })
                }}
              />
            )
          }}
        />
      )}
      {isCloud && directoryKey == null && hasPasteData && (
        <MenuEntry
          hidden={hidden}
          action="paste"
          doAction={() => {
            unsetModal()
            doPaste(rootDirectory.value.id)
          }}
        />
      )}
    </ContextMenu>
  )
}
