/** @file Modal for setting the organization name. */
import { Alert } from '#/components/Alert'
import { AlertDialog } from '#/components/AlertDialog'
import { Text } from '#/components/Text'
import * as backend from '#/services/Backend'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useBackends } from '$/providers/backends'
import { useText } from '$/providers/react'

/** Props for a {@link TrialEndedModal}. */
export interface TrialEndedModalProps {
  readonly subscriptionId: backend.SubscriptionId
}

/** The mutation data for the `createCheckoutSession` mutation. */
interface CancelSubcriptionMutationParams {
  readonly subscriptionId: backend.SubscriptionId
}

/** Modal for handling subscription after the trial period ended. */
export function TrialEndedModal(props: TrialEndedModalProps) {
  const { subscriptionId } = props
  const { getText } = useText()
  const { remoteBackend } = useBackends()

  const onConfirm = useMutationCallback({
    mutationFn: async () => {
      const { url } = await remoteBackend.createCheckoutSession({
        price: backend.Plan.solo,
        quantity: 1,
        interval: 1,
        flowType: backend.BillingPortalFlowType.paymentMethodUpdate,
      })
      window.open(url, '_blank')?.focus()
    },
  })

  const onCancel = useMutationCallback({
    mutationFn: async (mutationData: CancelSubcriptionMutationParams) => {
      await remoteBackend.cancelSubscription(mutationData.subscriptionId)
    },
  })

  return (
    <AlertDialog
      title={getText('trialEnded')}
      modalProps={{ defaultOpen: true }}
      cancel={getText('downgrade')}
      confirm={getText('subscribe')}
      onConfirm={onConfirm}
      onCancel={() => onCancel({ subscriptionId })}
    >
      <Text className="relative">{getText('trialEndedExplenation')}</Text>

      <Alert variant="outline" icon="warning">
        {getText('trialEndedWarning')}
      </Alert>
    </AlertDialog>
  )
}
