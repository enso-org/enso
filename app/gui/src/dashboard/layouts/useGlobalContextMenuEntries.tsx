/** @file Context menu entries available everywhere in the directory. */
import { backendMutationOptions, useNewFolder, useNewProject } from '#/hooks/backendHooks'
import { useUploadFiles } from '#/hooks/backendUploadFilesHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { defineMenuEntries } from '#/hooks/menuHooks'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import { CreateCredentialModal } from '#/modals/CreateCredentialModal'
import UpsertDatalinkModal from '#/modals/UpsertDatalinkModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'
import { useDriveStore } from '#/providers/DriveProvider'
import { setModal } from '#/providers/ModalProvider'
import type Backend from '#/services/Backend'
import { BackendType, type DirectoryId } from '#/services/Backend'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useStore } from '#/utilities/zustand'
import { readUserSelectedFile } from 'enso-common/src/utilities/file'

/** Props for a {@link GlobalContextMenuEntries}. */
export interface GlobalContextMenuEntriesOptions {
  readonly backend: Backend
  readonly category: Category
  readonly currentDirectoryId: DirectoryId
  readonly directoryId: DirectoryId | null
  readonly doPaste: (newParentKey: DirectoryId, newParentId: DirectoryId) => void
}

/** Context menu entries available everywhere in the directory. */
export function useGlobalContextMenuEntries(options: GlobalContextMenuEntriesOptions) {
  const { backend, category, directoryId = null, currentDirectoryId, doPaste } = options

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
  const newProject = useEventCallback(() => newProjectRaw({}, directoryId ?? currentDirectoryId))
  const uploadFilesRaw = useUploadFiles(backend, category)
  const uploadFiles = useEventCallback((files: readonly File[]) =>
    uploadFilesRaw(files, directoryId ?? currentDirectoryId),
  )

  return defineMenuEntries([
    {
      action: 'uploadFiles',
      doAction: () => {
        void readUserSelectedFile().then((files) => uploadFiles(Array.from(files)))
      },
    },
    {
      action: 'newProject',
      doAction: () => {
        void newProject()
      },
    },
    {
      action: 'newFolder',
      doAction: () => {
        void newFolder()
      },
    },
    isCloud && {
      action: 'newSecret',
      doAction: () => {
        setModal(
          <UpsertSecretModal
            doCreate={async (name, value) => {
              await newSecret([
                { name, value, parentDirectoryId: directoryId ?? currentDirectoryId },
              ])
            }}
          />,
        )
      },
    },
    isCloud && {
      action: 'newCredential',
      doAction: () => {
        setModal(
          <CreateCredentialModal
            doCreate={async (name, value) =>
              await newCredential([
                { name, value, parentDirectoryId: directoryId ?? currentDirectoryId },
              ])
            }
          />,
        )
      },
    },
    isCloud && {
      action: 'newDatalink',
      doAction: () => {
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
      },
    },
    hasPasteData &&
      directoryId == null && {
        action: 'paste',
        doAction: () => {
          doPaste(currentDirectoryId, currentDirectoryId)
        },
      },
  ])
}
