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
import type { TextId } from 'enso-common/src/text'
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

/** Functions to manipulate notification state. */
export interface NotificationStateControls
  extends Omit<ReturnType<typeof useNotificationState>, 'computedNotifications'> {}

/** Return notification state and a set of functions to control it. */
export function useNotificationState() {
  const [notificationMap, setNotificationMap] = useState<ReadonlyMap<unknown, NotificationInfo>>(
    new Map(),
  )

  const getComputedNotification = useEventCallback((key: unknown) => {
    return notificationMap.get(key)
  })

  const removeComputedNotification = useEventCallback((id: string) => {
    setNotificationMap((map) => new Map([...map.entries()].filter(([, v]) => v.id !== id)))
  })

  const upsertComputedNotification = useEventCallback(
    (key: unknown, newNotification: NotificationInfo) => {
      setNotificationMap((map) => {
        const newNotifications = new Map(map)
        const existingNotification = map.get(key)
        const notification: NotificationInfo = {
          ...newNotification,
          timestamp:
            existingNotification?.timestamp ?? newNotification.timestamp ?? Number(new Date()),
        }
        newNotifications.set(key, notification)
        const isFinished = (() => {
          if (!('progress' in notification) || notification.progress == null) {
            // If the notification does not have a progress value, assume it is instantaneous
            // (or finished by the time it was added).
            return true
          }
          if (notification.progress === 'indeterminate') {
            // An `indeterminate` progress means that the action is still ongoing.
            return false
          }
          // Else a notification is finished if its progress is 1.
          return notification.progress >= 1
        })()
        if (notification.showToast === true) {
          if (!existingNotification) {
            const toastFunction = isFinished ? toast.success : toast.loading
            toastFunction(<NotificationItem {...omit(notification, 'timestamp', 'progress')} />, {
              position: 'bottom-right',
              toastId: notification.id,
              closeButton: true,
              ...('progress' in notification && notification.progress != null ?
                { progress: notification.progress }
              : {}),
            })
          } else {
            toast.update(notification.id, {
              type: isFinished ? 'success' : 'default',
              isLoading: !isFinished,
              autoClose: null,
              render: () => <NotificationItem {...omit(notification, 'timestamp', 'progress')} />,
              progress: notification.progress ?? null,
            })
          }
        }
        if (isFinished) {
          setTimeout(() => {
            removeComputedNotification(newNotification.id)
          }, COMPUTED_NOTIFICATION_STORAGE_TIME_MS)
        }
        return newNotifications
      })
    },
  )

  const computedNotifications: readonly NotificationInfo[] = [...notificationMap.values()].reverse()

  return {
    computedNotifications,
    getComputedNotification,
    upsertComputedNotification,
    removeComputedNotification,
  }
}

/** Options for {@link useComputedNotifications}. */
export interface UseComputedNotificationsOptions extends NotificationStateControls {}

/** Return a list of transient notification details. */
export function useComputedNotifications(options: UseComputedNotificationsOptions) {
  const { getComputedNotification, upsertComputedNotification } = options
  const { getText } = useText()

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
    const existingNotification = getComputedNotification(uploadingFilesEntries[0][0])
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
      upsertComputedNotification(uploadingFilesEntries[0][0], {
        id: uploadingFilesEntries[0][0],
        message: newMessage,
        icon: 'data_upload',
        ...(sentFiles !== totalFiles ? { progress: sentBytes / totalBytes } : {}),
        showToast: true,
      })
    }
  }
}

/** Options for {@link useMoreComputedNotificationsIfEnabled}. */
export interface UseMoreComputedNotificationsIfEnabledOptions extends NotificationStateControls {}

/** Return a list of transient notification details. */
export function useMoreComputedNotificationsIfEnabled(
  options: UseMoreComputedNotificationsIfEnabledOptions,
) {
  const { upsertComputedNotification } = options
  const queryClient = useQueryClient()
  const { getText } = useText()
  const moreComputedNotifications = useFeatureFlag('moreComputedNotifications')

  useEffect(() => {
    if (!moreComputedNotifications) {
      return
    }
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
              const messageId = ((): TextId => {
                if (force) {
                  if (isSuccess) {
                    return 'permanentlyDeletedXAssetsNotification'
                  }
                  if (isError) {
                    return 'couldNotPermanentlyDeleteXAssetsNotification'
                  }
                  return 'permanentlyDeletingXAssetsNotification'
                } else {
                  if (isSuccess) {
                    return 'deletedXAssetsNotification'
                  } else if (isError) {
                    return 'couldNotDeleteXAssetsNotification'
                  }
                  return 'deletingXAssetsNotification'
                }
              })()
              upsertComputedNotification(variables, {
                id: upsertMutationId(variables),
                message: getText(messageId, ids.length),
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
              const messageId = (() => {
                if (isSuccess) {
                  return 'restoredXAssetsNotification'
                }
                if (isError) {
                  return 'couldNotRestoreXAssetsNotification'
                }
                return 'restoringXAssetsNotification'
              })()
              upsertComputedNotification(variables, {
                id: upsertMutationId(variables),
                message: getText(messageId, variables.length),
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
              const messageId = (() => {
                if (isSuccess) {
                  return 'copiedXAssetsNotification'
                }
                if (isError) {
                  return 'couldNotCopyXAssetsNotification'
                }
                return 'copyingXAssetsNotification'
              })()
              upsertComputedNotification(variables, {
                id: upsertMutationId(variables),
                message: getText(messageId, variables[0].length),
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
              const messageId = (() => {
                if (isSuccess) {
                  return 'movedXAssetsNotification'
                }
                if (isError) {
                  return 'couldNotMoveXAssetsNotification'
                }
                return 'movingXAssetsNotification'
              })()
              upsertComputedNotification(variables, {
                id: upsertMutationId(variables),
                message: getText(messageId, variables[0].length),
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
  }, [getText, moreComputedNotifications, queryClient, upsertComputedNotification])
}
