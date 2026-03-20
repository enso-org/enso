<script lang="ts">
import RegistrationReact from '#/pages/authentication/Registration'
import type { DataLoader } from '$/router'
import { reactComponent } from '@/util/react'
import { Ok } from 'enso-common/src/utilities/data/result'

const Registration = reactComponent(RegistrationReact)

// eslint-disable-next-line @typescript-eslint/no-empty-object-type
type Props = {}

export const dataLoader: DataLoader<Props> = {
  async beforeRouteEnter() {
    return Ok({})
  },
}
</script>

<script setup lang="ts">
defineProps<Props>()
</script>

<template>
  <Registration />
</template>
