/** @file Modal for handling user subscription after initial trial ended. */
import { Alert } from '#/components/Alert'
import { AlertDialog } from '#/components/AlertDialog'
import { Text } from '#/components/Text'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useLocalStorageState } from '#/hooks/localStoreState'
import LocalStorage from '#/utilities/LocalStorage'
import { DAY_MS, HOUR_MS, HOURS_PER_DAY, MINUTE_MS, useCurrentTimestamp } from '#/utilities/time'
import { useText } from '$/providers/react'
import { z } from 'zod'

/** Props for a {@link PlanDowngradedModal}. */
export interface PlanDowngradedModalProps {
  readonly deletionDeadlineTimestamp: number
}

declare module '#/utilities/LocalStorage' {
  /** Metadata containing the last time user has acknowledged the modal with asset removal deadline. */
  interface LocalStorageData {
    readonly downgradeModal: z.infer<typeof STORAGE_SCHEMA>
  }
}

const STORAGE_SCHEMA = z.object({
  lastShownTimestamp: z.number(),
})

LocalStorage.registerKey('downgradeModal', {
  isUserSpecific: true,
  schema: STORAGE_SCHEMA,
})

/** Minimum amount of time that must pass before the modal is shown again. */
const MIN_SHOW_INTERVAL = HOUR_MS
/** Maximum amount of time that must pass before the modal is shown again. */
const MAX_SHOW_INTERVAL = DAY_MS * 5

/** Hook for accessing or setting storage state related to downgrade modal. */
// eslint-disable-next-line react-refresh/only-export-components
export function useDowngadeModalState() {
  const [storageState, setStorageState] = useLocalStorageState('downgradeModal')

  const markAsShown = useEventCallback(() => {
    setStorageState({ lastShownTimestamp: Date.now() })
  })

  return {
    lastShownTimestamp: storageState?.lastShownTimestamp ?? 0,
    markAsShown,
  }
}

/** Modal for handling subscription after the trial period ended. */
export function PlanDowngradedModal(props: PlanDowngradedModalProps) {
  const { deletionDeadlineTimestamp } = props
  const { getText } = useText()

  const { lastShownTimestamp, markAsShown } = useDowngadeModalState()

  // Progress time reference every minute, so we can show the modal as time goes on.
  const referenceNowTime = useCurrentTimestamp(MINUTE_MS)

  const msToDeadline = deletionDeadlineTimestamp - referenceNowTime
  const daysLeft = Math.floor(msToDeadline / DAY_MS)
  const hoursLeft = Math.floor(msToDeadline / HOUR_MS) % HOURS_PER_DAY

  // Show alert again if the time elapsed since last showing is greater than the time left to delete,
  // That way the alerts become more frequent as the deadline approaches. Limited by set min/max range.
  const showInterval = Math.min(MAX_SHOW_INTERVAL, Math.max(msToDeadline, MIN_SHOW_INTERVAL))
  const timeSinceLastShow = referenceNowTime - lastShownTimestamp
  const modalDueToShow = timeSinceLastShow > showInterval

  const isOpen = msToDeadline > 0 && modalDueToShow

  return (
    <AlertDialog
      title={getText('downgradedTitle')}
      modalProps={{ isOpen }}
      onConfirm={markAsShown}
      cancel={null}
    >
      <Text className="relative">{getText('downgradedExplanation')}</Text>
      <Alert variant="outline" icon="warning">
        {getText('downgradedWarning', daysLeft, hoursLeft)}
      </Alert>
    </AlertDialog>
  )
}
