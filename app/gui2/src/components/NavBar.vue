<script setup lang="ts">
import NavBreadcrumbs from '@/components/NavBreadcrumbs.vue'
</script>

<template>
  <div class="NavBar">
    <NavBreadcrumbs />
  </div>
</template>

<style scoped>
.NavBar {
  user-select: none;
  display: flex;
  border-radius: var(--radius-full);
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
  place-items: center;
  gap: 12px;
  padding-left: 8px;
  padding-right: 10px;
  padding-top: 4px;
  padding-bottom: 4px;
}
</style>
