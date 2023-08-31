<script setup lang="ts">
import NavBar from '@/components/NavBar.vue'
import type { Breadcrumb } from '@/components/NavBreadcrumb.vue'

const props = defineProps<{ breadcrumbs: Breadcrumb[] }>()
</script>

<template>
  <div class="TopBar">
    <NavBar :breadcrumbs="breadcrumbs" />
  </div>
</template>

<style scoped>
.TopBar {
  display: flex;
  gap: 8px;
}
</style>
