/** @file A page for when a subscription payment succeeds. */
import { Button } from '#/components/Button'
import { Loader } from '#/components/Loader'
import Page from '#/components/Page'
import { useMount } from '#/hooks/mountHooks'
import {
  clearPendingCheckoutTargetPlan,
  getPendingCheckoutTargetPlan,
} from '#/modules/payments/pendingCheckout'
import { DASHBOARD_PATH } from '$/appUtils'
import { useAuth } from '$/providers/auth'
import { useRouter, useText } from '$/providers/react'
import * as analytics from '$/utils/analytics'
import { useQueryClient } from '@tanstack/react-query'
import { BackendType } from 'enso-common/src/services/Backend'
import { useEffect, useRef } from 'react'
import { toast } from 'react-toastify'

const USER_REFETCH_DELAY_MS = 3_000
const USER_REFETCH_TIMEOUT_MS = 60_000

/** A page for when a subscription payment succeeds. */
export function PaymentsSuccess() {
  const { router } = useRouter()
  const queryClient = useQueryClient()
  const { getText } = useText()
  const { refetchSession } = useAuth()
  const isMounted = useRef(true)

  useEffect(() => {
    isMounted.current = true

    return () => {
      isMounted.current = false
    }
  })

  useMount(() => {
    const promise = (async () => {
      const targetPlan = getPendingCheckoutTargetPlan()
      if (targetPlan == null) {
        await router.push(DASHBOARD_PATH)
        return
      }

      const startEpochMs = Number(new Date())
      // Extracted into a function to disable flow typing, because `isMounted.current` may be mutated.
      const getIsMounted = () => isMounted.current

      while (true) {
        if (!getIsMounted()) {
          throw new Error('Operation cancelled.')
        }
        const { data: session } = await refetchSession()
        if (!getIsMounted()) {
          throw new Error('Operation cancelled.')
        }
        if (session && 'user' in session && session.user.plan === targetPlan) {
          clearPendingCheckoutTargetPlan()
          // Invalidate "users me" query as the user has changed the plan.
          await queryClient.invalidateQueries({
            queryKey: [BackendType.remote, 'usersMe'],
          })

          await router.push(DASHBOARD_PATH)
          break
        } else {
          const timePassedMs = Number(new Date()) - startEpochMs
          if (timePassedMs > USER_REFETCH_TIMEOUT_MS) {
            await router.push(DASHBOARD_PATH)
            throw new Error(
              'Timed out waiting for subscription, please contact support to continue.',
            )
          } else {
            await new Promise((resolve) => {
              window.setTimeout(resolve, USER_REFETCH_DELAY_MS)
            })
          }
        }
      }

      analytics.checkout.after()
    })()

    void toast.promise(promise, {
      pending: getText('paymentsSuccessPending'),
      success: getText('paymentsSuccessSuccess'),
      error: getText('paymentsSuccessError'),
    })
  })

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
