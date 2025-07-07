<script setup lang="ts">
import { SetupOrganizationAfterSubscribe as SetupOrganizationAfterSubscribeReact } from '#/modals/SetupOrganizationAfterSubscribe'
import { useBackends } from '$/providers/backends'
import { useEvent } from '@/composables/events'
import { reactComponent } from '@/util/react'
import { onMounted, onUnmounted } from 'vue'
const SetupOrganizationAfterSubscribe = reactComponent(SetupOrganizationAfterSubscribeReact)

const { remoteBackend } = useBackends()
const logUserOpen = () => remoteBackend.logEvent('open_app')
const logUserClose = () => remoteBackend.logEvent('close_app')
onMounted(logUserOpen)
onUnmounted(logUserClose)
useEvent(window, 'beforeunload', logUserClose)
</script>

<template>
  <SetupOrganizationAfterSubscribe />
  <RouterView />
</template>
