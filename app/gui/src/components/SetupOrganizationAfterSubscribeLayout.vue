<script lang="ts">
import { SetupOrganizationAfterSubscribeProps } from '#/modals/SetupOrganizationAfterSubscribe'
import * as backendModule from '#/services/Backend'
import { useAuth, UserSessionType } from '$/providers/auth'
import { useBackends } from '$/providers/backends'
import { backendQueryOptions } from '@/composables/backend'
import * as vueQuery from '@tanstack/vue-query'
import { computed, effectScope, ref } from 'vue'

const PLANS_TO_SPECIFY_ORG_NAME = [backendModule.Plan.team, backendModule.Plan.enterprise]

export default {
  async beforeRouteEnter(_to, _from, next) {
    const scope = effectScope()
    const { remoteBackend: backend } = useBackends()
    const auth = useAuth()
    const queryClient = vueQuery.useQueryClient()
    if (auth.session?.type !== UserSessionType.full) return

    const { user } = session
    const { isOrganizationAdmin, userId, plan = backendModule.Plan.free } = user

    if (!PLANS_TO_SPECIFY_ORG_NAME.includes(plan) && isOrganizationAdmin) return

    const wojtas = vueQuery.useQuery(backendQueryOptions('getOrganization', [], backend))

    const { organizationName, userGroupsCount } = vueQuery.useQueries({
      queries: [
        backendQueryOptions('getOrganization', [], backend),
        backendQueryOptions('listUserGroups', [], backend),
      ],
      combine: ([organizationQuery, userGroupsQuery]) => ({
        organizationName: computed(() => organizationQuery.data.value?.name ?? null),
        userGroupsCount: computed(() => userGroupsQuery.data.value.length),
      }),
    })
  },
}
</script>

<script setup lang="ts">
const modalProps = ref<SetupOrganizationAfterSubscribeProps>()

defineExpose({ modalProps })
</script>
