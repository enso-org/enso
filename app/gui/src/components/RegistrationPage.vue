<script lang="ts">
import RegistrationReact from '#/pages/authentication/Registration'
import { useUserAgreements } from '$/composables/userAgreements'
import type { DataLoader } from '$/router'
import { Ok } from '@/util/data/result'
import { reactComponent } from '@/util/react'
import { useQueryClient } from '@tanstack/vue-query'

const Registration = reactComponent(RegistrationReact)

export const dataLoader: DataLoader<{ userAgreedFn: () => void }> = {
  beforeRouteEnter() {
    const queryClient = useQueryClient()
    return useUserAgreements(queryClient).then(({ userAgreed }) => Ok({ userAgreedFn: userAgreed }))
  },
}
</script>

<script setup lang="ts">
defineProps<{ userAgreedFn: () => void }>()
</script>

<template>
  <Registration :userAgreed="userAgreedFn" />
</template>
