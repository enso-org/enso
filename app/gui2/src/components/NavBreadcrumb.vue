<script setup lang="ts">
const props = defineProps<{ text: string; active: boolean }>()
</script>

<template>
  <div :class="['NavBreadcrumb', { inactive: !props.active }]">
    <span v-text="props.text"></span>
  </div>
</template>

<style scoped>
span {
  display: inline-block;
  height: 24px;
  padding: 1px 0px;
}

.NavBreadcrumb {
  user-select: none;
  cursor: pointer;
  border-radius: var(--radius-full);

  > .blur-container {
    border-radius: var(--radius-full);
    background-color: var(--color-frame-bg);
    backdrop-filter: var(--backdrop-blur);
  }
}

.inactive {
  opacity: 0.4;
}
</style>
