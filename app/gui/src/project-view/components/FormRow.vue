<script setup lang="ts">
const { inline = false } = defineProps<{
  inline?: boolean
}>()
</script>

<template>
  <div class="FormRow" :class="{ inline }">
    <label class="label"><slot name="label" /></label>
    <div class="element"><slot /></div>
  </div>
</template>

<style scoped>
.FormRow {
  display: flex;
  flex-direction: column;
  gap: 4px;

  .label {
    font-weight: bold;
  }

  &.inline {
    flex-direction: row;
    gap: 8px;

    .label {
      line-height: var(--node-port-height);
    }
  }
}
</style>
