<script lang="ts">
import { InvitedToOrganizationModal as InvitedToOrganizationModalReact } from '#/modals/InvitedToOrganizationModal'
import {
  SetupOrganizationAfterSubscribeProps,
  SetupOrganizationAfterSubscribe as SetupOrganizationAfterSubscribeReact,
} from '#/modals/SetupOrganizationAfterSubscribe'
import * as backendModule from '#/services/Backend'
import { useAuth, UserSessionType } from '$/providers/auth'
import { useBackends } from '$/providers/backends'
import { backendQueryOptions } from '@/composables/backend'
import { useEvent } from '@/composables/events'
import { reactComponent } from '@/util/react'
import * as vueQuery from '@tanstack/vue-query'
import { onMounted, onUnmounted, ref } from 'vue'
const InvitedToOrganizationModal = reactComponent(InvitedToOrganizationModalReact)
const SetupOrganizationAfterSubscribe = reactComponent(SetupOrganizationAfterSubscribeReact)

const PLANS_TO_SPECIFY_ORG_NAME = [backendModule.Plan.team, backendModule.Plan.enterprise]

/**
 * A layout for "main app" router views.
 *
 * TODO[ao]: should be merged with `AppContainer` probably, but first we need to remove
 * the "Dashboard" layer between them.
 */
export default {
  async beforeRouteEnter(_to, _from, next) {
    const { remoteBackend: backend } = useBackends()
    const auth = useAuth()
    const queryClient = vueQuery.useQueryClient()

    if (auth.session?.type !== UserSessionType.full) return next()
    const { isOrganizationAdmin, userId, plan = backendModule.Plan.free } = auth.session.user
    if (!(PLANS_TO_SPECIFY_ORG_NAME.includes(plan) && isOrganizationAdmin)) return next()

    const organizationName = await queryClient.fetchQuery(
      backendQueryOptions('getOrganization', [], backend),
    )
    const fetchedUserGroups = await queryClient.fetchQuery(
      backendQueryOptions('listUserGroups', [], backend),
    )

    next((component) => {
      component.setupOrganizationModalProps = { userId, organizationName, fetchedUserGroups }
    })
  },
}
</script>

<script setup lang="ts">
const { remoteBackend } = useBackends()

const logUserOpen = () => remoteBackend.logEvent('open_app')
const logUserClose = () => remoteBackend.logEvent('close_app')
onMounted(logUserOpen)
onUnmounted(logUserClose)
useEvent(window, 'beforeunload', logUserClose)

const setupOrganizationModalProps = ref<SetupOrganizationAfterSubscribeProps>()
defineExpose({ setupOrganizationModalProps })
</script>

<template>
  <SetupOrganizationAfterSubscribe
    v-if="setupOrganizationModalProps"
    v-bind="setupOrganizationModalProps"
  />
  <RouterView />
  <InvitedToOrganizationModal />
</template>
