/** @file Modal for handling user subscription after initial trial ended. */
import { Alert } from '#/components/Alert'
import { AlertDialog } from '#/components/AlertDialog'
import { Text } from '#/components/Text'
import { backendMutationOptions } from '#/hooks/backendHooks'
import * as backend from '#/services/Backend'
import { useBackends } from '$/providers/backends'
import { useText } from '$/providers/react'
import { useMutation } from '@tanstack/react-query'

/** Props for a {@link TrialEndedModal}. */
export interface TrialEndedModalProps {
  readonly subscriptionId: backend.SubscriptionId
}

/** Modal for handling subscription after the trial period ended. */
export function TrialEndedModal(props: TrialEndedModalProps) {
  const { subscriptionId } = props
  const { getText } = useText()
  const { remoteBackend } = useBackends()

  const onConfirm = useMutation(
    backendMutationOptions(remoteBackend, 'createCheckoutSession', {
      onSuccess: ({ url }) => window.open(url, '_blank'),
    }),
  )

  const onCancel = useMutation(backendMutationOptions(remoteBackend, 'cancelSubscription'))

  return (
    <AlertDialog
      title={getText('trialEnded')}
      modalProps={{ defaultOpen: true }}
      cancel={getText('downgrade')}
      confirm={getText('subscribe')}
      onConfirm={() => {
        onConfirm.mutate([
          {
            price: backend.Plan.solo,
            quantity: 1,
            interval: 1,
          },
        ])
      }}
      onCancel={() => {
        onCancel.mutate([subscriptionId])
      }}
    >
      <Text className="relative">{getText('trialEndedExplanation')}</Text>

      <Alert variant="outline" icon="warning">
        {getText('trialEndedWarning')}
      </Alert>
    </AlertDialog>
  )
}
