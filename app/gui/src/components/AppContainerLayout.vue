<script lang="ts">
import { SetupOrganizationAfterSubscribe as SetupOrganizationAfterSubscribeReact } from '#/modals/SetupOrganizationAfterSubscribe'
import * as backendModule from '#/services/Backend'
import { useAuth } from '$/providers/auth'
import { useBackends } from '$/providers/backends'
import type { DataLoader } from '$/router'
import { useEvent } from '@/composables/events'
import { reactComponent } from '@/util/react'
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
  showSetupOrganizationModal?: true
}> = {
  async beforeRouteEnter() {
    const auth = useAuth()
    if (!auth.session) return Ok({})
    const { isOrganizationAdmin, plan = backendModule.Plan.free } = auth.session.user
    if (!(PLANS_TO_SPECIFY_ORG_NAME.includes(plan) && isOrganizationAdmin)) return Ok({})
    return Ok({ showSetupOrganizationModal: true })
  },
}
</script>

<script setup lang="ts">
defineProps<{ showSetupOrganizationModal?: true }>()

const { remoteBackend } = useBackends()
const logUserOpen = () => remoteBackend.logEvent('open_app')
const logUserClose = () => remoteBackend.logEvent('close_app')
onMounted(logUserOpen)
onUnmounted(logUserClose)
useEvent(window, 'beforeunload', logUserClose)
</script>

<template>
  <SetupOrganizationAfterSubscribe v-if="showSetupOrganizationModal" />
  <RouterView />
</template>
