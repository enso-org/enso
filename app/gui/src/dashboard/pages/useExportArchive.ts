/** @file A React hook returning a function to export an archive. */
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useDownloadDirectory } from '#/layouts/Drive/useDownloadDirectory'
import { useDriveStore } from '#/providers/DriveProvider'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useText } from '$/providers/react'
import { PRODUCT_NAME } from 'enso-common/src/constants'
import type { Backend } from 'enso-common/src/services/Backend'
import { Path } from 'enso-common/src/services/Backend'
import { toReadableIsoString } from 'enso-common/src/utilities/data/dateTime'
import { toast } from 'react-toastify'

/** Options for {@link useExportArchive}. */
export interface ExportArchiveOptions {
  readonly backend: Backend
}

/** Return a function to export an archive. */
export function useExportArchive(options: ExportArchiveOptions) {
  const { backend } = options

  const { getText } = useText()
  const exportArchive = useMutationCallback(backendMutationOptions(backend, 'exportArchive'))
  const driveStore = useDriveStore()
  const downloadDirectory = useDownloadDirectory()

  return useEventCallback(async () => {
    const { selectedIds } = driveStore.getState()
    const secondsString = new Date().getSeconds().toString().padStart(2, '0')
    const dateString = `${toReadableIsoString(new Date()).replace(/[:]/g, ' ')} ${secondsString}`
    const [filePathRaw] =
      (await window.api?.fileBrowser.openFileBrowser(
        'filePath',
        `${downloadDirectory}/${PRODUCT_NAME} ${dateString}.zip`,
      )) ?? []
    if (window.api && filePathRaw == null) {
      // Assume that the user cancelled the action.
      return
    }
    // If the file path is null, assume the user is using the desktop app's server with a browser.
    // The desktop app's server will return a stream instead that can be downloaded by the browser.
    const filePath = filePathRaw != null ? Path(filePathRaw) : null
    await toast.promise(exportArchive([{ assetIds: [...selectedIds], filePath }]), {
      pending: getText('exportArchive.inProgress'),
      success: getText('exportArchive.success'),
      error: getText('exportArchive.failure'),
    })
  })
}
