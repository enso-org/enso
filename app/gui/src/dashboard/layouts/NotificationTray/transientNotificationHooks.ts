/** @file Hooks for computing temporary notifications. */
import CopyIcon from '#/assets/copy.svg'
import UploadIcon from '#/assets/data_upload.svg'
import MoveIcon from '#/assets/duplicate.svg'
import DeleteIcon from '#/assets/trash.svg'
import UntrashIcon from '#/assets/untrash.svg'
import {
  copyAssetsMutationKey,
  deleteAssetsMutationKey,
  moveAssetsMutationKey,
  restoreAssetsMutationKey,
} from '#/hooks/backendBatchedHooks'
import { MB_BYTES, uploadingFileQueryOptions } from '#/hooks/backendUploadFilesHooks'
import { useText } from '#/providers/TextProvider'
import { useIsMutating, useQuery, type MutationKey } from '@tanstack/react-query'
import { BackendType } from 'enso-common/src/services/Backend'
import type { NotificationInfo } from './types'

/** Return the number of ongoing mutations of the given type across both backends. */
export function useIsMutatingForBothBackends(makeKey: (backendType: BackendType) => MutationKey) {
  return (
    useIsMutating({ mutationKey: makeKey(BackendType.local) }) +
      useIsMutating({ mutationKey: makeKey(BackendType.remote) }) !==
    0
  )
}

/** Return a list of transient notification details. */
export function useTransientNotifications(): readonly NotificationInfo[] {
  const { getText } = useText()
  const { data: uploadingFiles } = useQuery(uploadingFileQueryOptions())

  const notifications: NotificationInfo[] = []

  const isDeletingAssets = useIsMutatingForBothBackends(deleteAssetsMutationKey)
  if (isDeletingAssets) {
    notifications.push({
      id: 'temporary-delete-assets',
      message: getText('deletingAssetsNotification'),
      icon: DeleteIcon,
      color: 'danger',
    })
  }

  const isRestoringAssets = useIsMutatingForBothBackends(restoreAssetsMutationKey)
  if (isRestoringAssets) {
    notifications.push({
      id: 'temporary-restore-assets',
      message: getText('restoringAssetsNotification'),
      icon: UntrashIcon,
    })
  }

  const isCopyingAssets = useIsMutatingForBothBackends(copyAssetsMutationKey)
  if (isCopyingAssets) {
    notifications.push({
      id: 'temporary-copy-assets',
      message: getText('copyingAssetsNotification'),
      icon: CopyIcon,
    })
  }

  const isMovingAssets = useIsMutatingForBothBackends(moveAssetsMutationKey)
  if (isMovingAssets) {
    notifications.push({
      id: 'temporary-move-assets',
      message: getText('movingAssetsNotification'),
      icon: MoveIcon,
    })
  }

  const uploadingFilesEntries = Object.entries(uploadingFiles)
  if (uploadingFilesEntries.length !== 0) {
    const totalFiles = uploadingFilesEntries.length
    let sentBytes = 0
    let totalBytes = 0
    for (const [, progress] of uploadingFilesEntries) {
      sentBytes += progress.sentBytes
      totalBytes += progress.totalBytes
    }
    const sentMb = sentBytes / MB_BYTES
    const totalMb = totalBytes / MB_BYTES
    notifications.push({
      id: 'temporary-uploading-files',
      message: getText('uploadingXFilesWithProgressNotification', totalFiles, sentMb, totalMb),
      icon: UploadIcon,
    })
  }

  return notifications
}
