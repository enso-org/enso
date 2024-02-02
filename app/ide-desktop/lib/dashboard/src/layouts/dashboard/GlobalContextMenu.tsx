/** @file A context menu available everywhere in the directory. */
import * as React from 'react'

import type * as assetListEventModule from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'
import UpsertSecretModal from '#/layouts/dashboard/UpsertSecretModal'
import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import type * as backendModule from '#/services/backend'
import * as shortcuts from '#/utilities/shortcuts'

import ContextMenu from '#/components/ContextMenu'
import MenuEntry from '#/components/MenuEntry'

/** Props for a {@link GlobalContextMenu}. */
export interface GlobalContextMenuProps {
  hidden?: boolean
  isCloud: boolean
  hasPasteData: boolean
  directoryKey: backendModule.DirectoryId | null
  directory: backendModule.SmartDirectory | null
  dispatchAssetListEvent: (event: assetListEventModule.AssetListEvent) => void
  doPaste: (newParentKey: backendModule.AssetId) => void
}

/** A context menu available everywhere in the directory. */
export default function GlobalContextMenu(props: GlobalContextMenuProps) {
  const { hidden = false, isCloud, hasPasteData, directoryKey, directory } = props
  const { dispatchAssetListEvent, doPaste } = props
  const { organization } = authProvider.useNonPartialUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const rootDirectory = React.useMemo(() => organization?.rootDirectory(), [organization])
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
        ></input>
      )}
      <MenuEntry
        hidden={hidden}
        action={
          isCloud ? shortcuts.KeyboardAction.uploadFiles : shortcuts.KeyboardAction.uploadProjects
        }
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
          action={shortcuts.KeyboardAction.newProject}
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
          action={shortcuts.KeyboardAction.newFolder}
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
          action={shortcuts.KeyboardAction.newDataConnector}
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
      {isCloud && directoryKey == null && hasPasteData && (
        <MenuEntry
          hidden={hidden}
          action={shortcuts.KeyboardAction.paste}
          doAction={() => {
            unsetModal()
            doPaste(rootDirectory.value.id)
          }}
        />
      )}
    </ContextMenu>
  )
}
