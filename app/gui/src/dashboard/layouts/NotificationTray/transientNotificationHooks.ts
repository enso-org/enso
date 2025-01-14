/** @file Hooks for computing temporary notifications. */
import CopyIcon from '#/assets/copy.svg'
import MoveIcon from '#/assets/duplicate.svg'
import DeleteIcon from '#/assets/trash.svg'
import UntrashIcon from '#/assets/untrash.svg'
import {
  copyAssetsMutationKey,
  deleteAssetsMutationKey,
  moveAssetsMutationKey,
  restoreAssetsMutationKey,
} from '#/hooks/backendBatchedHooks'
import { useText } from '#/providers/TextProvider'
import { useIsMutating, type MutationKey } from '@tanstack/react-query'
import { BackendType } from 'enso-common/src/services/Backend'
import type { NotificationInfo } from './types'

/** Return the number of ongoing mutations of the given type across both backends. */
export function useIsMutatingForBothBackends(makeKey: (backendType: BackendType) => MutationKey) {
  return (
    useIsMutating({ mutationKey: makeKey(BackendType.local) }) +
    useIsMutating({ mutationKey: makeKey(BackendType.remote) })
  )
}

/** Return a list of transient notification details. */
export function useTransientNotifications(): readonly NotificationInfo[] {
  const { getText } = useText()

  const notifications: NotificationInfo[] = []

  const deleteAssetsCount = useIsMutatingForBothBackends(deleteAssetsMutationKey)
  if (deleteAssetsCount > 0) {
    notifications.push({
      id: 'temporary-delete-assets',
      message: getText('deletingXAssetsNotification', deleteAssetsCount),
      icon: DeleteIcon,
      color: 'danger',
    })
  }

  const restoreAssetsCount = useIsMutatingForBothBackends(restoreAssetsMutationKey)
  if (restoreAssetsCount > 0) {
    notifications.push({
      id: 'temporary-restore-assets',
      message: getText('restoringXAssetsNotification', restoreAssetsCount),
      icon: UntrashIcon,
    })
  }

  const copyAssetsCount = useIsMutatingForBothBackends(copyAssetsMutationKey)
  if (copyAssetsCount > 0) {
    notifications.push({
      id: 'temporary-copy-assets',
      message: getText('copyingXAssetsNotification', copyAssetsCount),
      icon: CopyIcon,
    })
  }

  const moveAssetsCount = useIsMutatingForBothBackends(moveAssetsMutationKey)
  if (moveAssetsCount > 0) {
    notifications.push({
      id: 'temporary-move-assets',
      message: getText('movingXAssetsNotification', moveAssetsCount),
      icon: MoveIcon,
    })
  }

  return notifications
}
