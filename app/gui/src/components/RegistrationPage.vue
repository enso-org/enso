<script lang="ts">
import RegistrationReact from '#/pages/authentication/Registration'
import { useUserAgreements } from '$/composables/userAgreements'
import type { DataLoader } from '$/router'
import { reactComponent } from '@/util/react'
import { useQueryClient } from '@tanstack/vue-query'
import { Ok } from 'enso-common/src/utilities/data/result'

const Registration = reactComponent(RegistrationReact)

type Props = { userAgreedFn: () => void }

export const dataLoader: DataLoader<Props> = {
  async beforeRouteEnter() {
    const queryClient = useQueryClient()
    const agreements = await useUserAgreements(queryClient)
    // With no readable agreements there are no version hashes to record, but registration itself
    // must still work.
    return Ok({ userAgreedFn: agreements?.userAgreed ?? (() => {}) })
  },
}
</script>

<script setup lang="ts">
defineProps<Props>()
</script>

<template>
  <Registration :userAgreed="userAgreedFn" />
</template>
