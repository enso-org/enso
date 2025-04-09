<script lang="ts">
import Backend from '#/services/Backend'
import { identity } from '#/utilities/functions'
import { createContextStore } from '@/providers'

interface Backends {
  remoteBackend: Backend | null
  localBackend: Backend | null
}
export const [provideBackend, injectBackendInVue] = createContextStore(
  'backend-from-react',
  identity<Backends>,
)
</script>

<script setup lang="ts">
const props = defineProps<Backends>()

provideBackend(props)
</script>

<template>
  <slot />
</template>
