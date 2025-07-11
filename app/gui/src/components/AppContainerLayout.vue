<script lang="ts">
import { SetupOrganizationModal as SetupOrganizationModalReact } from '#/modals/SetupOrganizationForm'
import * as backendModule from '#/services/Backend'
import { useAuth } from '$/providers/auth'
import { useBackends } from '$/providers/backends'
import type { DataLoader } from '$/router'
import { backendQueryOptions } from '@/composables/backend'
import { useEvent } from '@/composables/events'
import { reactComponent } from '@/util/react'
import { useQuery } from '@tanstack/vue-query'
import { computed, onMounted, onUnmounted } from 'vue'
import { Ok } from 'ydoc-shared/util/data/result'

const SetupOrganizationModal = reactComponent(SetupOrganizationModalReact)

const PLANS_TO_SPECIFY_ORG_NAME = [backendModule.Plan.team, backendModule.Plan.enterprise]

/**
 * A layout for "main app" router views.
 *
 * TODO[ao]: should be merged with `AppContainer` probably, but first we need to remove
 * the "Dashboard" layer between them.
 */
export const dataLoader: DataLoader<{
  shouldSetupOrganization?: boolean
}> = {
  async beforeRouteEnter() {
    const auth = useAuth()
    const { remoteBackend: backend } = useBackends()
    if (!auth.session) return Ok({})
    const { isOrganizationAdmin, plan = backendModule.Plan.free } = auth.session.user
    if (!(PLANS_TO_SPECIFY_ORG_NAME.includes(plan) && isOrganizationAdmin)) return Ok({})
    const organizationQuery = useQuery(backendQueryOptions('getOrganization', [], backend))
    await organizationQuery.suspense()
    return Ok({
      shouldSetupOrganization: computed(
        () =>
          organizationQuery.data.value?.name == null || organizationQuery.data.value?.name === '',
      ),
    })
  },
}
</script>

<script setup lang="ts">
defineProps<{ shouldSetupOrganization?: boolean }>()

const { remoteBackend } = useBackends()
const logUserOpen = () => remoteBackend.logEvent('open_app')
const logUserClose = () => remoteBackend.logEvent('close_app')
onMounted(logUserOpen)
onUnmounted(logUserClose)
useEvent(window, 'beforeunload', logUserClose)
</script>

<template>
  <SetupOrganizationModal v-if="shouldSetupOrganization" />
  <RouterView />
</template>
