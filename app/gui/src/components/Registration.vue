<script lang="ts">
import RegistrationReact from '#/pages/authentication/Registration'
import { useUserAgrements } from '$/composables/userAgreements'
import { reactComponent } from '@/util/react'
import * as vueQuery from '@tanstack/vue-query'
import { EffectScope, effectScope, ref } from 'vue'

const Registration = reactComponent(RegistrationReact)

export default {
  async beforeRouteEnter(_to, _from, next) {
    const queryClient = vueQuery.useQueryClient()

    const scope = effectScope()
    const userAgreements = await scope.run(() => useUserAgrements(queryClient))
    return next((component) => {
      component.routeScope = scope
      component.userAgreedFn = userAgreements?.userAgreed
    })
  },
  dataLoad() {},
  beforeRouteLeave() {
    this.routeScope?.stop()
  },
}
</script>

<script setup lang="ts">
const userAgreedFn = ref<() => void>()

defineExpose({
  routeScope: undefined as EffectScope | undefined,
  userAgreedFn,
})
</script>

<template>
  <Registration v-if="userAgreedFn" :userAgreed="userAgreedFn" />
</template>
