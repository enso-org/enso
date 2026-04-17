<script setup lang="ts">
import ActionButton from '@/components/ActionButton.vue'
</script>

<template>
  <div class="ProjectLogToolbar">
    <ActionButton action="sessionLogs.reload" />
    <ActionButton action="sessionLogs.loadMore" />
    <ActionButton action="sessionLogs.download" />
  </div>
</template>

<style scoped>
.ProjectLogToolbar {
  display: flex;
  justify-content: flex-end;
  gap: 4px;
}
</style>
