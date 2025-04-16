<script lang="ts">
import Backend from '#/services/Backend'
import { identity } from '#/utilities/functions'
import { createContextStore } from '@/providers'
import { markRaw, proxyRefs, toRaw, toRef } from 'vue'

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

provideBackend(
  proxyRefs({
    remoteBackend: toRef(() => props.remoteBackend && markRaw(toRaw(props.remoteBackend))),
    localBackend: toRef(() => props.localBackend && markRaw(toRaw(props.localBackend))),
  }),
)
</script>

<template>
  <slot />
</template>
