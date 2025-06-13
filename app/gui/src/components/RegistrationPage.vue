<script lang="ts">
import RegistrationReact from '#/pages/authentication/Registration'
import { useUserAgrements } from '$/composables/userAgreements'
import { reactComponent } from '@/util/react'

const Registration = reactComponent(RegistrationReact)

export default {
  dataLoader(queryClient) {
    return useUserAgrements(queryClient).then(({ userAgreed }) => userAgreed)
  },
}
</script>

<script setup lang="ts">
defineProps<{ userAgreedFn: () => void }>()
</script>

<template>
  <Registration v-if="userAgreedFn" :userAgreed="userAgreedFn" />
</template>
