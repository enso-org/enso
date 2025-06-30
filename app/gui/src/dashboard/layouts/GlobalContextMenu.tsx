/** @file A context menu available everywhere in the directory. */
import ContextMenuEntry from '#/components/ContextMenuEntry'
import { Separator } from '#/components/Separator'
import { backendMutationOptions, useNewFolder, useNewProject } from '#/hooks/backendHooks'
import { useUploadFiles } from '#/hooks/backendUploadFilesHooks'
import { usePaste } from '#/hooks/cutAndPasteHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useStore } from '#/hooks/storeHooks'
import { canTransferBetweenCategories, type Category } from '#/layouts/CategorySwitcher/Category'
import { CreateCredentialModal } from '#/modals/CreateCredentialModal'
import UpsertDatalinkModal from '#/modals/UpsertDatalinkModal'
import UpsertSecretModal from '#/modals/UpsertSecretModal'
import { useDriveStore, useSelectedAssets, useSetPasteData } from '#/providers/DriveProvider'
import { setModal, unsetModal } from '#/providers/ModalProvider'
import {
  AssetType,
  BackendType,
  type AnyAsset,
  type default as Backend,
  type DirectoryId,
} from '#/services/Backend'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { readUserSelectedFile } from 'enso-common/src/utilities/file'
import { toast } from 'react-toastify'

/** Props for a {@link GlobalContextMenu}. */
export interface GlobalContextMenuProps {
  readonly hidden?: boolean
  readonly backend: Backend
  readonly category: Category
  readonly currentDirectoryId: DirectoryId
  readonly asset: AnyAsset | null
  readonly bindingFocusScope?: React.RefObject<HTMLElement> | undefined
}

/** A context menu available everywhere in the directory. */
export const GlobalContextMenu = function GlobalContextMenu(props: GlobalContextMenuProps) {
  // For some reason, applying the ReactCompiler for this component breaks the copy-paste functionality
  // eslint-disable-next-line react-compiler/react-compiler
  'use no memo'

  const { hidden = false, backend, category, asset, currentDirectoryId, bindingFocusScope } = props

  const directoryId = asset?.type === AssetType.directory ? asset.id : null

  const isCloud = backend.type === BackendType.remote
  const canCreateItems = category.type !== 'trash' && category.type !== 'recent'

  const paste = usePaste(category)
  const setPasteData = useSetPasteData()
  const doPaste = useEventCallback((newParentId: DirectoryId) => {
    unsetModal()

    const { pasteData } = driveStore.getState()

    if (!pasteData) {
      return
    }

    if (pasteData.data.assets.some((otherAsset) => otherAsset.id === newParentId)) {
      toast.error('Cannot paste a folder into itself.')
      return
    }

    void paste({
      fromCategory: pasteData.data.category,
      toCategory: category,
      newParentId,
      pasteData: pasteData.data,
      method: pasteData.type,
    })

    setPasteData(null)
  })

  const driveStore = useDriveStore()
  const hasPasteData = useStore(driveStore, (store) => {
    const pasteData =
      (
        store.pasteData?.data.backendType === backend.type &&
        canTransferBetweenCategories(
          store.pasteData.data.category,
          category,
          directoryId ?? currentDirectoryId,
        )
      ) ?
        store.pasteData
      : null
    return (pasteData?.data.assets.length ?? 0) > 0
  })

  const selectedAssets = useSelectedAssets()

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

  return (
    <>
      {hasPasteData && category.type !== 'recent' && (
        <ContextMenuEntry
          bindingFocusScope={bindingFocusScope}
          hidden={hidden}
          action="paste"
          doAction={() => {
            const [selected] = selectedAssets
            const targetDirectoryId =
              selectedAssets.length === 1 && selected?.type === AssetType.directory ?
                selected.id
              : currentDirectoryId
            doPaste(targetDirectoryId)
          }}
        />
      )}
      {canCreateItems && !hidden && <Separator className="my-0.5 first:hidden" />}
      {canCreateItems && (
        <ContextMenuEntry
          bindingFocusScope={bindingFocusScope}
          hidden={hidden}
          action="uploadFiles"
          doAction={async () => {
            const files = await readUserSelectedFile()
            void uploadFiles(Array.from(files))
          }}
        />
      )}
      {canCreateItems && (
        <ContextMenuEntry
          bindingFocusScope={bindingFocusScope}
          hidden={hidden}
          action="newProject"
          doAction={() => {
            unsetModal()
            void newProject(null, null)
          }}
        />
      )}
      {canCreateItems && (
        <ContextMenuEntry
          bindingFocusScope={bindingFocusScope}
          hidden={hidden}
          action="newFolder"
          doAction={() => {
            unsetModal()
            void newFolder()
          }}
        />
      )}
      {canCreateItems && isCloud && (
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
      {canCreateItems && isCloud && (
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
      {canCreateItems && isCloud && (
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
    </>
  )
}
