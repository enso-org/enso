<script lang="ts">
import {
  CreateUserGroupModal as CreateUserGroupModalReact,
  SetOrganizationNameModal as SetOrganizationNameModalReact,
} from '#/modals/SetupOrganizationAfterSubscribe'
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

const SetOrganizationNameModal = reactComponent(SetOrganizationNameModalReact)
const CreateUserGroupModal = reactComponent(CreateUserGroupModalReact)

const PLANS_TO_SPECIFY_ORG_NAME = [backendModule.Plan.team, backendModule.Plan.enterprise]

/**
 * A layout for "main app" router views.
 *
 * TODO[ao]: should be merged with `AppContainer` probably, but first we need to remove
 * the "Dashboard" layer between them.
 */
export const dataLoader: DataLoader<{
  shouldSetOrganizationName?: boolean
  shouldSetDefaultUserGroup?: boolean
}> = {
  async beforeRouteEnter() {
    const auth = useAuth()
    const { remoteBackend: backend } = useBackends()
    if (!auth.session) return Ok({})
    const { isOrganizationAdmin, plan = backendModule.Plan.free } = auth.session.user
    if (!(PLANS_TO_SPECIFY_ORG_NAME.includes(plan) && isOrganizationAdmin)) return Ok({})
    const organizationQuery = useQuery(backendQueryOptions('getOrganization', [], backend))
    const userGroupsQuery = useQuery(backendQueryOptions('listUserGroups', [], backend))
    await Promise.all([organizationQuery.suspense(), userGroupsQuery.suspense()])
    return Ok({
      shouldSetOrganizationName: computed(
        () =>
          organizationQuery.data.value?.name == null || organizationQuery.data.value?.name === '',
      ),
      shouldSetDefaultUserGroup: computed(() => (userGroupsQuery.data.value?.length ?? 0) === 0),
    })
  },
}
</script>

<script setup lang="ts">
defineProps<{ shouldSetOrganizationName?: boolean; shouldSetDefaultUserGroup?: boolean }>()

const { remoteBackend } = useBackends()
const logUserOpen = () => remoteBackend.logEvent('open_app')
const logUserClose = () => remoteBackend.logEvent('close_app')
onMounted(logUserOpen)
onUnmounted(logUserClose)
useEvent(window, 'beforeunload', logUserClose)
</script>

<template>
  <SetOrganizationNameModal v-if="shouldSetOrganizationName" />
  <CreateUserGroupModal v-else-if="shouldSetDefaultUserGroup" />
  <RouterView />
</template>
