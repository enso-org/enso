<script setup lang="js">
import Dashboard from '#/pages/dashboard/Dashboard'
import { applyPureReactInVue } from 'veaury'
import { onMounted, onUnmounted } from 'vue'

const ReactDashboard = applyPureReactInVue(Dashboard)

onMounted(() => {
  console.error('Vue Dashboard MOUNT')
})
onUnmounted(() => {
  console.error('Vue Dashboard UNMOUNT')
})
</script>

<template>
  <ReactDashboard />
</template>
