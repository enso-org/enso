/** @file A context menu available everywhere in the directory. */
import { useStore } from '#/utilities/zustand'

import ContextMenu from '#/components/ContextMenu'
import ContextMenuEntry from '#/components/ContextMenuEntry'

import UpsertDatalinkModal from '#/modals/UpsertDatalinkModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'

import { backendMutationOptions, useNewFolder, useNewProject } from '#/hooks/backendHooks'
import { useUploadFiles } from '#/hooks/backendUploadFilesHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import { CreateCredentialModal } from '#/modals/CreateCredentialModal'
import { useDriveStore } from '#/providers/DriveProvider'
import { setModal, unsetModal } from '#/providers/ModalProvider'
import type Backend from '#/services/Backend'
import { BackendType, type DirectoryId } from '#/services/Backend'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useText } from '$/providers/react'
import { readUserSelectedFile } from 'enso-common/src/utilities/file'

/** Props for a {@link GlobalContextMenu}. */
export interface GlobalContextMenuProps {
  /** If true, returns a list of components rather than a {@link ContextMenu}. */
  readonly noWrapper?: boolean
  readonly hidden?: boolean
  readonly backend: Backend
  readonly category: Category
  readonly currentDirectoryId: DirectoryId
  readonly directoryId: DirectoryId | null
  readonly doPaste: (newParentKey: DirectoryId, newParentId: DirectoryId) => void
  readonly event: Pick<React.MouseEvent, 'pageX' | 'pageY'>
  readonly bindingFocusScope?: React.RefObject<HTMLElement> | undefined
}

/** A context menu available everywhere in the directory. */
export const GlobalContextMenu = function GlobalContextMenu(props: GlobalContextMenuProps) {
  // For some reason, applying the ReactCompiler for this component breaks the copy-paste functionality
  // eslint-disable-next-line react-compiler/react-compiler
  'use no memo'

  const {
    noWrapper = false,
    hidden = false,
    backend,
    category,
    directoryId = null,
    currentDirectoryId,
    event,
    doPaste,
    bindingFocusScope,
  } = props

  const { getText } = useText()
  const isCloud = backend.type === BackendType.remote

  const driveStore = useDriveStore()
  const hasPasteData = useStore(
    driveStore,
    (storeState) => (storeState.pasteData?.data.assets.length ?? 0) > 0,
  )

  const newFolderRaw = useNewFolder(backend, category)
  const newFolder = useEventCallback(async () => {
    return await newFolderRaw(directoryId ?? currentDirectoryId)
  })
  const newSecret = useMutationCallback(backendMutationOptions(backend, 'createSecret'))
  const newCredential = useMutationCallback(backendMutationOptions(backend, 'createCredential'))
  const newDatalink = useMutationCallback(backendMutationOptions(backend, 'createDatalink'))
  const newProjectRaw = useNewProject(backend, category)
  const newProject = useEventCallback(
    async (templateId: string | null | undefined, templateName: string | null | undefined) => {
      return await newProjectRaw({ templateName, templateId }, directoryId ?? currentDirectoryId)
    },
  )
  const uploadFilesRaw = useUploadFiles(backend, category)
  const uploadFiles = useEventCallback(async (files: readonly File[]) => {
    await uploadFilesRaw(files, directoryId ?? currentDirectoryId)
  })

  const entries = (
    <>
      <ContextMenuEntry
        bindingFocusScope={bindingFocusScope}
        hidden={hidden}
        action="uploadFiles"
        doAction={async () => {
          const files = await readUserSelectedFile()
          await uploadFiles(Array.from(files))
        }}
      />
      <ContextMenuEntry
        bindingFocusScope={bindingFocusScope}
        hidden={hidden}
        action="newProject"
        doAction={() => {
          unsetModal()
          void newProject(null, null)
        }}
      />
      <ContextMenuEntry
        bindingFocusScope={bindingFocusScope}
        hidden={hidden}
        action="newFolder"
        doAction={() => {
          unsetModal()
          void newFolder()
        }}
      />
      {isCloud && (
        <ContextMenuEntry
          bindingFocusScope={bindingFocusScope}
          hidden={hidden}
          action="newSecret"
          doAction={() => {
            setModal(
              <UpsertSecretModal
                doCreate={async (name, value) => {
                  await newSecret([
                    { name, value, parentDirectoryId: directoryId ?? currentDirectoryId },
                  ])
                }}
              />,
            )
          }}
        />
      )}
      {isCloud && (
        <ContextMenuEntry
          bindingFocusScope={bindingFocusScope}
          hidden={hidden}
          action="newCredential"
          doAction={() => {
            setModal(
              <CreateCredentialModal
                doCreate={async (name, value) =>
                  await newCredential([
                    { name, value, parentDirectoryId: directoryId ?? currentDirectoryId },
                  ])
                }
              />,
            )
          }}
        />
      )}
      {isCloud && (
        <ContextMenuEntry
          bindingFocusScope={bindingFocusScope}
          hidden={hidden}
          action="newDatalink"
          doAction={() => {
            setModal(
              <UpsertDatalinkModal
                doCreate={async (name, value) => {
                  await newDatalink([
                    {
                      name,
                      value,
                      parentDirectoryId: directoryId ?? currentDirectoryId,
                      datalinkId: null,
                    },
                  ])
                }}
              />,
            )
          }}
        />
      )}
      {hasPasteData && directoryId == null && (
        <ContextMenuEntry
          bindingFocusScope={bindingFocusScope}
          hidden={hidden}
          action="paste"
          doAction={() => {
            unsetModal()
            doPaste(currentDirectoryId, currentDirectoryId)
          }}
        />
      )}
    </>
  )

  return noWrapper ? entries : (
      <ContextMenu aria-label={getText('globalContextMenuLabel')} hidden={hidden} event={event}>
        {entries}
      </ContextMenu>
    )
}
