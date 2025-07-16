/** @file A page for when a subscription payment succeeds. */
import { Loader } from '#/components/Loader'
import Page from '#/components/Page'
import { useMount } from '#/hooks/mountHooks'
import { BackendType, Plan } from '#/services/Backend'
import { DASHBOARD_PATH } from '$/appUtils'
import { useAuth } from '$/providers/auth'
import { useRouter, useText, useUserSession } from '$/providers/react'
import * as analytics from '$/utils/analytics'
import { useQueryClient } from '@tanstack/react-query'
import { toast } from 'react-toastify'

const USER_REFETCH_DELAY_MS = 3_000
const USER_REFETCH_TIMEOUT_MS = 30_000

/** A page for when a subscription payment succeeds. */
export function PaymentsSuccess() {
  const { router } = useRouter()
  const queryClient = useQueryClient()
  const { getText } = useText()
  const { refetchSession } = useAuth()
  const oldSession = useUserSession()

  useMount(() => {
    const promise = (async () => {
      const startEpochMs = Number(new Date())

      while (true) {
        const { data: session } = await refetchSession()
        if (
          session &&
          'user' in session &&
          session.user.plan !==
            (oldSession && 'user' in oldSession ? oldSession.user.plan : Plan.free)
        ) {
          // Invalidate "users me" query as the user has changed the plan.
          await queryClient.invalidateQueries({
            queryKey: [BackendType.remote, 'usersMe'],
          })

          await router.push(DASHBOARD_PATH)
          break
        } else {
          const timePassedMs = Number(new Date()) - startEpochMs
          if (timePassedMs > USER_REFETCH_TIMEOUT_MS) {
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
      <Loader className="h-full w-full" />
    </Page>
  )
}
