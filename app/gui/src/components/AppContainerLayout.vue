<script lang="ts">
import {
  SetupOrganizationAfterSubscribeProps,
  SetupOrganizationAfterSubscribe as SetupOrganizationAfterSubscribeReact,
} from '#/modals/SetupOrganizationAfterSubscribe'
import * as backendModule from '#/services/Backend'
import { useAuth } from '$/providers/auth'
import { useBackends } from '$/providers/backends'
import type { DataLoader } from '$/router'
import { backendQueryOptions } from '@/composables/backend'
import { useEvent } from '@/composables/events'
import { reactComponent } from '@/util/react'
import { useQueryClient } from '@tanstack/vue-query'
import { onMounted, onUnmounted } from 'vue'
import { Ok } from 'ydoc-shared/util/data/result'
const SetupOrganizationAfterSubscribe = reactComponent(SetupOrganizationAfterSubscribeReact)

const PLANS_TO_SPECIFY_ORG_NAME = [backendModule.Plan.team, backendModule.Plan.enterprise]

/**
 * A layout for "main app" router views.
 *
 * TODO[ao]: should be merged with `AppContainer` probably, but first we need to remove
 * the "Dashboard" layer between them.
 */
export const dataLoader: DataLoader<{
  setupOrganizationModalProps?: SetupOrganizationAfterSubscribeProps
}> = {
  async beforeRouteEnter() {
    const queryClient = useQueryClient()
    const auth = useAuth()
    const { remoteBackend: backend } = useBackends()
    if (!auth.session) return Ok({})
    const { isOrganizationAdmin, userId, plan = backendModule.Plan.free } = auth.session.user
    if (!(PLANS_TO_SPECIFY_ORG_NAME.includes(plan) && isOrganizationAdmin)) return Ok({})
    const [organization, fetchedUserGroups] = await Promise.all([
      queryClient.fetchQuery(backendQueryOptions('getOrganization', [], backend)),
      queryClient.fetchQuery(backendQueryOptions('listUserGroups', [], backend)),
    ])
    return Ok({
      setupOrganizationModalProps: {
        userId,
        organizationName: organization?.name ?? null,
        userGroupsCount: fetchedUserGroups.length,
      },
    })
  },
}
</script>

<script setup lang="ts">
defineProps<{ setupOrganizationModalProps?: SetupOrganizationAfterSubscribeProps }>()

const { remoteBackend } = useBackends()
const logUserOpen = () => remoteBackend.logEvent('open_app')
const logUserClose = () => remoteBackend.logEvent('close_app')
onMounted(logUserOpen)
onUnmounted(logUserClose)
useEvent(window, 'beforeunload', logUserClose)
</script>

<template>
  <SetupOrganizationAfterSubscribe
    v-if="setupOrganizationModalProps"
    v-bind="setupOrganizationModalProps"
  />
  <RouterView />
</template>
