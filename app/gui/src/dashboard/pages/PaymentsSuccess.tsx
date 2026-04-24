/** @file A page for when a subscription payment succeeds. */
import { Button } from '#/components/Button'
import { Loader } from '#/components/Loader'
import Page from '#/components/Page'
import { AbortError } from '#/hooks/timeoutHooks'
import {
  clearPendingCheckoutTargetPlan,
  getPendingCheckoutTargetPlan,
} from '#/modules/payments/pendingCheckout'
import { DASHBOARD_PATH } from '$/appUtils'
import { useAuth } from '$/providers/auth'
import { useRouter, useText } from '$/providers/react'
import * as analytics from '$/utils/analytics'
import { useQueryClient } from '@tanstack/react-query'
import type { Plan } from 'enso-common/src/services/Backend'
import { BackendType } from 'enso-common/src/services/Backend'
import { wait } from 'lib0/promise'
import { useEffect } from 'react'
import { toast } from 'react-toastify'

const USER_REFETCH_DELAY_MS = 3_000
const TIMEOUT = 60_000

/** Wait for user's subscription plan to update. */
async function waitForPlan(
  plan: Plan,
  refetchSession: ReturnType<typeof useAuth>['refetchSession'],
  abort: AbortSignal,
) {
  const timeout = Number(new Date()) + TIMEOUT
  while (Number(new Date()) < timeout && !abort.aborted) {
    const { data } = await refetchSession()
    if (data != null && data.user.plan === plan) {
      return 'success'
    }
    await wait(USER_REFETCH_DELAY_MS)
  }
  if (abort.aborted) {
    throw new AbortError('abort')
  }
  return 'timeout'
}

/** A page for when a subscription payment succeeds. */
export function PaymentsSuccess() {
  const { router } = useRouter()
  const queryClient = useQueryClient()
  const { getText } = useText()
  const { refetchSession } = useAuth()

  useEffect(() => {
    const plan = getPendingCheckoutTargetPlan()
    if (plan == null) {
      void router.push(DASHBOARD_PATH)
      return
    }

    const abortController = new AbortController()
    const waitForPlanPromise = async () => {
      const loadingToast = toast.loading(getText('payments.pending'))
      try {
        const result = await waitForPlan(plan, refetchSession, abortController.signal)
        switch (result) {
          case 'success':
            clearPendingCheckoutTargetPlan()
            await queryClient.invalidateQueries({
              queryKey: [BackendType.remote, 'getOrganization'],
            })
            toast.success(getText('payments.success'))
            analytics.checkout.after()
            await router.push(DASHBOARD_PATH)
            break
          case 'timeout':
            clearPendingCheckoutTargetPlan()
            toast.error(getText('payments.timeout'))
            await router.push(DASHBOARD_PATH)
            break
        }
      } catch (e) {
        if (e instanceof AbortError) {
          return
        }
        toast.error(getText('payments.error'))
        // eslint-disable-next-line no-restricted-properties
        console.error(e)
      } finally {
        toast.dismiss(loadingToast)
      }
    }

    void waitForPlanPromise()

    return () => {
      abortController.abort()
    }
  }, [getText, queryClient, refetchSession, router])

  return (
    <Page>
      <Loader className="h-full w-full">
        <Button
          variant="delete"
          onPress={async () => {
            clearPendingCheckoutTargetPlan()
            await router.push(DASHBOARD_PATH)
          }}
        >
          {getText('cancel')}
        </Button>
      </Loader>
    </Page>
  )
}
