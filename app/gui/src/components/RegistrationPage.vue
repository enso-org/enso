<script lang="ts">
import RegistrationReact from '#/pages/authentication/Registration'
import { useUserAgreements } from '$/composables/userAgreements'
import type { DataLoader } from '$/router'
import { Ok } from '@/util/data/result'
import { reactComponent } from '@/util/react'
import { useQueryClient } from '@tanstack/vue-query'

const Registration = reactComponent(RegistrationReact)

export const dataLoader: DataLoader<{ userAgreedFn: () => void }> = {
  async beforeRouteEnter() {
    const queryClient = useQueryClient()
    const { userAgreed } = await useUserAgreements(queryClient)
    return Ok({ userAgreedFn: userAgreed })
  },
}
</script>

<script setup lang="ts">
defineProps<{ userAgreedFn: () => void }>()
</script>

<template>
  <Registration :userAgreed="userAgreedFn" />
</template>
