/** @file Hooks for computing temporary notifications. */
import {
  COPY_ASSETS_MUTATION_METHOD,
  DELETE_ASSETS_MUTATION_METHOD,
  MOVE_ASSETS_MUTATION_METHOD,
  RESTORE_ASSETS_MUTATION_METHOD,
  type MutationFromOptionsFunction,
  type copyAssetsMutationOptions,
  type deleteAssetsMutationOptions,
  type moveAssetsMutationOptions,
  type restoreAssetsMutationOptions,
} from '#/hooks/backendBatchedHooks'
import { MB_BYTES, uploadingFileQueryOptions } from '#/hooks/backendUploadFilesHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useFeatureFlag } from '#/providers/FeatureFlagsProvider'
import { useText } from '#/providers/TextProvider'
import { useIsMutating, useQuery, useQueryClient, type MutationKey } from '@tanstack/react-query'
import { BackendType } from 'enso-common/src/services/Backend'
import { omit } from 'enso-common/src/utilities/data/object'
import { uniqueString } from 'enso-common/src/utilities/uniqueString'
import { useEffect, useState } from 'react'
import { toast } from 'react-toastify'
import { NotificationItem } from './NotificationItem'
import type { NotificationInfo } from './types'

const COMPUTED_NOTIFICATION_STORAGE_TIME_MS = 60_000
const MUTATION_ID_MAP = new WeakMap<object, string>()

/** Get or insert a mutation id for a computed mutation. */
function upsertMutationId(variables: object) {
  const id = MUTATION_ID_MAP.get(variables)
  if (id != null) {
    return id
  }
  const newId = uniqueString()
  MUTATION_ID_MAP.set(variables, newId)
  return newId
}

/** Return the number of ongoing mutations of the given type across both backends. */
export function useIsMutatingForBothBackends(makeKey: (backendType: BackendType) => MutationKey) {
  return (
    useIsMutating({ mutationKey: makeKey(BackendType.local) }) +
      useIsMutating({ mutationKey: makeKey(BackendType.remote) }) !==
    0
  )
}

/** Return a list of transient notification details. */
export function useComputedNotifications() {
  const queryClient = useQueryClient()
  const { getText } = useText()

  const moreComputedNotifications = useFeatureFlag('moreComputedNotifications')

  const [notificationMap, setNotificationMap] = useState<ReadonlyMap<unknown, NotificationInfo>>(
    new Map(),
  )

  const removeComputedNotification = useEventCallback((id: string) => {
    setNotificationMap((map) => new Map([...map.entries()].filter(([, v]) => v.id !== id)))
  })

  const upsertNotification = useEventCallback((key: unknown, newNotification: NotificationInfo) => {
    setNotificationMap((map) => {
      const newNotifications = new Map(map)
      const existingNotification = map.get(key)
      const notification: NotificationInfo = {
        ...newNotification,
        timestamp:
          existingNotification?.timestamp ?? newNotification.timestamp ?? Number(new Date()),
      }
      newNotifications.set(key, notification)
      const isFinished =
        !('progress' in notification) ||
        (typeof notification.progress === 'number' && notification.progress >= 1)
      if (!existingNotification && notification.showToast === true) {
        const toastFunction = isFinished ? toast.success : toast.loading
        toastFunction(<NotificationItem {...omit(notification, 'timestamp', 'progress')} />, {
          position: 'bottom-right',
          toastId: notification.id,
          closeButton: true,
          ...('progress' in notification ? { progress: notification.progress } : {}),
        })
      }
      if (isFinished) {
        setTimeout(() => {
          removeComputedNotification(newNotification.id)
        }, COMPUTED_NOTIFICATION_STORAGE_TIME_MS)
      }
      return newNotifications
    })
  })

  useEffect(() => {
    if (moreComputedNotifications) {
      return queryClient.getMutationCache().subscribe((update) => {
        switch (update.type) {
          case 'added':
          case 'updated': {
            const mutationRaw = update.mutation
            const isSuccess = mutationRaw.state.status === 'success'
            const isError = mutationRaw.state.status === 'error'
            const isPending = mutationRaw.state.status === 'pending'
            const sharedProps = (
              isPending ?
                { progress: 'indeterminate' }
              : {}) satisfies Partial<NotificationInfo>
            switch (mutationRaw.options.mutationKey?.[1]) {
              case DELETE_ASSETS_MUTATION_METHOD: {
                // eslint-disable-next-line no-restricted-syntax
                const mutation = mutationRaw as MutationFromOptionsFunction<
                  typeof deleteAssetsMutationOptions
                >
                const variables = mutation.state.variables
                if (!variables) {
                  break
                }
                const [ids, force] = variables
                upsertNotification(variables, {
                  id: upsertMutationId(variables),
                  message:
                    force ?
                      getText(
                        isSuccess ? 'permanentlyDeletedXAssetsNotification'
                        : isError ? 'couldNotPermanentlyDeleteXAssetsNotification'
                        : 'permanentlyDeletingXAssetsNotification',
                        ids.length,
                      )
                    : getText(
                        isSuccess ? 'deletedXAssetsNotification'
                        : isError ? 'couldNotDeleteXAssetsNotification'
                        : 'deletingXAssetsNotification',
                        ids.length,
                      ),
                  icon: 'trash2',
                  color: 'danger',
                  ...sharedProps,
                })
                break
              }
              case RESTORE_ASSETS_MUTATION_METHOD: {
                // eslint-disable-next-line no-restricted-syntax
                const mutation = mutationRaw as MutationFromOptionsFunction<
                  typeof restoreAssetsMutationOptions
                >
                const variables = mutation.state.variables
                if (!variables) {
                  break
                }
                upsertNotification(variables, {
                  id: upsertMutationId(variables),
                  message: getText(
                    isSuccess ? 'restoredXAssetsNotification'
                    : isError ? 'couldNotRestoreXAssetsNotification'
                    : 'restoringXAssetsNotification',
                    variables.length,
                  ),
                  icon: 'restore',
                  ...sharedProps,
                })
                break
              }
              case COPY_ASSETS_MUTATION_METHOD: {
                // eslint-disable-next-line no-restricted-syntax
                const mutation = mutationRaw as MutationFromOptionsFunction<
                  typeof copyAssetsMutationOptions
                >
                const variables = mutation.state.variables
                if (!variables) {
                  break
                }
                upsertNotification(variables, {
                  id: upsertMutationId(variables),
                  message: getText(
                    isSuccess ? 'copiedXAssetsNotification'
                    : isError ? 'couldNotCopyXAssetsNotification'
                    : 'copyingXAssetsNotification',
                    variables[0].length,
                  ),
                  icon: 'copy',
                  ...sharedProps,
                })
                break
              }
              case MOVE_ASSETS_MUTATION_METHOD: {
                // eslint-disable-next-line no-restricted-syntax
                const mutation = mutationRaw as MutationFromOptionsFunction<
                  typeof moveAssetsMutationOptions
                >
                const variables = mutation.state.variables
                if (!variables) {
                  break
                }
                upsertNotification(variables, {
                  id: upsertMutationId(variables),
                  message: getText(
                    isSuccess ? 'movedXAssetsNotification'
                    : isError ? 'couldNotMoveXAssetsNotification'
                    : 'movingXAssetsNotification',
                    variables[0].length,
                  ),
                  icon: 'duplicate',
                  ...sharedProps,
                })
                break
              }
            }
            break
          }
          case 'removed':
          case 'observerAdded':
          case 'observerRemoved':
          case 'observerOptionsUpdated': {
            // Ignored.
            break
          }
        }
      })
    } else {
      return
    }
  }, [getText, moreComputedNotifications, queryClient, upsertNotification])

  const { data: uploadingFiles } = useQuery(uploadingFileQueryOptions())

  const uploadingFilesEntries = Object.entries(uploadingFiles)
  if (uploadingFilesEntries[0]) {
    const totalFiles = uploadingFilesEntries.length
    let sentFiles = 0
    let sentBytes = 0
    let totalBytes = 0
    for (const [, progress] of uploadingFilesEntries) {
      if (progress.sentBytes === progress.totalBytes) {
        sentFiles += 1
      }
      sentBytes += progress.sentBytes
      totalBytes += progress.totalBytes
    }
    const sentMb = sentBytes / MB_BYTES
    const totalMb = totalBytes / MB_BYTES
    const existingNotification = notificationMap.get(uploadingFilesEntries[0][0])
    const newMessage =
      sentFiles === totalFiles ?
        getText('uploadedXFilesNotification', totalFiles)
      : getText(
          'uploadingXFilesWithProgressNotification',
          sentFiles,
          totalFiles,
          sentMb < 1 ? sentMb.toFixed(2) : String(Math.ceil(sentMb)),
          totalMb < 1 ? totalMb.toFixed(2) : String(Math.ceil(totalMb)),
        )
    // Assume each file upload only participates in one notification.
    // This assumption means that each notification can be uniquely identified by its first upload.
    // There is guaranteed to be at least one upload by this point because of the
    // `uploadingFilesEntries[0]` condition above.
    // Only upsert if changed to avoid infinite loop.
    if (existingNotification?.message !== newMessage) {
      upsertNotification(uploadingFilesEntries[0][0], {
        id: uploadingFilesEntries[0][0],
        message: newMessage,
        icon: 'data_upload',
        ...(sentFiles !== totalFiles ? { progress: sentBytes / totalBytes } : {}),
        showToast: true,
      })
    }
  }

  const computedNotifications: readonly NotificationInfo[] = [...notificationMap.values()].reverse()

  for (const notification of computedNotifications) {
    if (notification.showToast === true) {
      const isFinished =
        !('progress' in notification) ||
        (typeof notification.progress === 'number' && notification.progress >= 1)
      toast.update(notification.id, {
        type: isFinished ? 'success' : 'default',
        isLoading: !isFinished,
        autoClose: null,
        render: () => <NotificationItem {...omit(notification, 'timestamp', 'progress')} />,
        progress: notification.progress ?? null,
      })
    }
  }

  return { computedNotifications, removeComputedNotification }
}
