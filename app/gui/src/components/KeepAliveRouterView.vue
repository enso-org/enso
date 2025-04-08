<script lang="ts">
interface Backends {
  remoteBackend: Backend
  localBackend: Backend
}
export const [provideBackend, injectBackendInVue] = createContextStore(
  'backend-from-react',
  identity<Backends>,
)
</script>

<script setup lang="ts">
import Backend from '#/services/Backend'
import { identity } from '#/utilities/functions'
import { createContextStore } from '@/providers'
import { onMounted, onUnmounted } from 'vue'
import { RouterView } from 'vue-router'

const props = defineProps<Backends>()

provideBackend(props)

onMounted(() => {
  console.error('MOUNT RouterView')
})

onUnmounted(() => {
  console.error('UNMOUNT RouterView')
})
</script>

<template>
  <RouterView v-slot="{ Component }">
    <KeepAlive>
      <component :is="Component" />
    </KeepAlive>
  </RouterView>
</template>
