/**
 * @file
 *
 * Offline Notification Manager component.
 *
 * This component is responsible for displaying a toast notification when the user goes offline or online.
 */

import * as React from 'react'

import * as offlineHooks from '#/hooks/offlineHooks'

import * as textProvider from '#/providers/TextProvider'

import * as toast from '#/components/Toast'

/**
 * Props for {@link OfflineNotificationManager}
 */
export interface OfflineNotificationManagerProps extends React.PropsWithChildren {}

/**
 * Context props for {@link OfflineNotificationManager}
 */
interface OfflineNotificationManagerContextProps {
  readonly isNested: boolean
  readonly toastId?: string
}

const OfflineNotificationManagerContext =
  React.createContext<OfflineNotificationManagerContextProps>({ isNested: false })

/**
 * Offline Notification Manager component.
 */
export function OfflineNotificationManager(props: OfflineNotificationManagerProps) {
  const { children } = props
  const toastId = 'offline'
  const { getText } = textProvider.useText()

  offlineHooks.useOfflineChange(isOffline => {
    toast.toast.dismiss(toastId)

    if (isOffline) {
      toast.toast.info(getText('offlineToastMessage'), {
        toastId,
        description: getText('offlineToastMessageDescription'),
      })
    } else {
      toast.toast.info(getText('onlineToastMessage'), { description: '' })
    }
  })

  return (
    <OfflineNotificationManagerContext.Provider value={{ isNested: true, toastId }}>
      {children}
    </OfflineNotificationManagerContext.Provider>
  )
}
