/** @file Hooks for computing temporary notifications. */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useText } from '$/providers/react'
import { useVueValue } from '$/providers/react/common'
import { useUploadsToCloudStore } from '$/providers/react/upload'
import { useIsMutating, type MutationKey } from '@tanstack/react-query'
import { BackendType } from 'enso-common/src/services/Backend'
import { omit } from 'enso-common/src/utilities/data/object'
import { useCallback, useState } from 'react'
import { toast } from 'react-toastify'
import { NotificationItem } from './NotificationItem'
import type { NotificationInfo } from './types'

const MB_BYTES = 1_000_000
const COMPUTED_NOTIFICATION_STORAGE_TIME_MS = 60_000

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

  const uploadsStore = useUploadsToCloudStore()
  const uploadingFiles = useVueValue(
    useCallback(() => [...uploadsStore.uploads.entries()], [uploadsStore.uploads]),
  )
  const uploadingFilesEntries = uploadingFiles.filter(([, data]) => data.kind === 'requestedByUser')

  if (uploadingFilesEntries[0]) {
    const totalFiles = uploadingFilesEntries.length
    let sentFiles = 0
    let sentBytes = 0
    let totalBytes = 0
    for (const [, data] of uploadingFilesEntries) {
      if (data.sentBytes === data.totalBytes) {
        sentFiles += 1
      }
      sentBytes += data.sentBytes
      totalBytes += data.totalBytes
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
