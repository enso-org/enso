<script lang="ts">
import {
  AcceptInvitationModal as AcceptInvitationModalReact,
  type AcceptInvitationModalProps,
} from '#/modals/AcceptInvitationModal'
import {
  PlanDowngradedModal as PlanDowngradedModalReact,
  type PlanDowngradedModalProps,
} from '#/modals/PlanDowngradedModal'
import { SetupOrganizationModal as SetupOrganizationModalReact } from '#/modals/SetupOrganizationForm'
import {
  TrialEndedModal as TrialEndedModalReact,
  type TrialEndedModalProps,
} from '#/modals/TrialEndedModal'
import { DAY_MS } from '#/utilities/time'
import { useAuth } from '$/providers/auth'
import { useBackends } from '$/providers/backends'
import type { DataLoader } from '$/router'
import { backendQueryOptions } from '@/composables/backend'
import { useEvent } from '@/composables/events'
import { reactComponent } from '@/util/react'
import { proxyRefs } from '@/util/reactivity'
import { waitForData } from '@/util/tanstack'
import { useQuery } from '@tanstack/vue-query'
import * as backendModule from 'enso-common/src/services/Backend'
import { Ok } from 'enso-common/src/utilities/data/result'
import { computed, onMounted, onUnmounted } from 'vue'

const SetupOrganizationModal = reactComponent(SetupOrganizationModalReact)
const TrialEndedModal = reactComponent(TrialEndedModalReact)
const PlanDowngradedModal = reactComponent(PlanDowngradedModalReact)
const AcceptInvitationModal = reactComponent(AcceptInvitationModalReact)

const PLANS_TO_SPECIFY_ORG_NAME = [backendModule.Plan.team, backendModule.Plan.enterprise]

type Props = {
  shouldSetupOrganization: boolean
  trialEndedModalProps: TrialEndedModalProps | undefined
  planDowngradedModalProps: PlanDowngradedModalProps | undefined
  acceptInvitationModalProps: AcceptInvitationModalProps | undefined
}

/** Days of asset retention after trial ends. */
const DAYS_BEFORE_DELETE = 90

/**
 * A layout for "main app" router views.
 *
 * TODO[ao]: should be merged with `AppContainer` probably, but first we need to remove
 * the "Dashboard" layer between them.
 */
export const dataLoader: DataLoader<Props> = {
  async beforeRouteEnter() {
    const auth = useAuth()
    const { remoteBackend: backend } = useBackends()

    const { isOrganizationAdmin, plan, invitation } = auth.session?.user ?? {
      isOrganizationAdmin: false,
      plan: backendModule.Plan.free,
    }

    const needsOrganizationSetup = PLANS_TO_SPECIFY_ORG_NAME.includes(plan)

    const organizationQuery = useQuery(backendQueryOptions('getOrganization', [], backend))
    await waitForData(organizationQuery)

    const acceptInvitationModalProps = computed(() => (invitation ? { invitation } : undefined))

    const trialEndedModalProps = computed<TrialEndedModalProps | undefined>(() => {
      if (plan == backendModule.Plan.free) return undefined

      const subscription = organizationQuery.data.value?.subscription
      if (subscription?.isPaused && subscription.id != null) {
        return { subscriptionId: subscription.id }
      }
      return undefined
    })

    const planDowngradedModalProps = computed<PlanDowngradedModalProps | undefined>(() => {
      if (plan != backendModule.Plan.free) return undefined
      const subscription = organizationQuery.data.value?.subscription
      if (subscription?.isPaused && subscription.id != null && subscription.trialEnd != null) {
        return {
          deletionDeadlineTimestamp:
            Number(new Date(subscription.trialEnd)) + DAYS_BEFORE_DELETE * DAY_MS,
        }
      }
      return undefined
    })

    const shouldSetupOrganization = computed(
      () => isOrganizationAdmin && needsOrganizationSetup && !organizationQuery.data.value?.name,
    )

    return Ok(
      proxyRefs({
        shouldSetupOrganization,
        trialEndedModalProps,
        planDowngradedModalProps,
        acceptInvitationModalProps,
      }),
    )
  },
}
</script>

<script setup lang="ts">
defineProps<Props>()

const { remoteBackend } = useBackends()
const logUserOpen = () => remoteBackend.logEvent('open_app')
const logUserClose = () => remoteBackend.logEvent('close_app')
onMounted(logUserOpen)
onUnmounted(logUserClose)
useEvent(window, 'beforeunload', logUserClose)
</script>

<template>
  <SetupOrganizationModal v-if="shouldSetupOrganization" />
  <TrialEndedModal v-if="trialEndedModalProps" v-bind="trialEndedModalProps" />
  <PlanDowngradedModal v-if="planDowngradedModalProps" v-bind="planDowngradedModalProps" />
  <AcceptInvitationModal v-if="acceptInvitationModalProps" v-bind="acceptInvitationModalProps" />
  <RouterView />
</template>
