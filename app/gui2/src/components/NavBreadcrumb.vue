<script setup lang="ts">
export interface Breadcrumb {
  text: string
  onClick: (payload: MouseEvent) => void
}

const props = defineProps<Breadcrumb>()
</script>

<template>
  <div class="TopBarBreadcrumb"><span v-text="text" :on-click="onClick"></span></div>
</template>

<style scoped>
.TopBarBreadcrumb {
  border-radius: var(--radius-full);
}

.TopBarBreadcrumb > .blur-container {
  border-radius: var(--radius-full);
  background-color: var(--color-frame-bg);
  backdrop-filter: var(--backdrop-blur);
}
</style>
