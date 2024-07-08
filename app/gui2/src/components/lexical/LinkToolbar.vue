<script setup lang="ts">
import { documentationEditorBindings } from '@/bindings'

const props = defineProps<{ url: string }>()
</script>

<template>
  <div class="LinkToolbar">
    <a :href="props.url" target="_blank" rel="nofollow">Follow link</a>
    <span class="shortcut"
      >({{ documentationEditorBindings.bindings.openLink.humanReadable }})</span
    >
  </div>
</template>

<style scoped>
.LinkToolbar {
  display: flex;
  background-color: white;
  border-radius: var(--radius-full);
  padding: 4px;
  gap: 4px;
}

a {
  color: steelblue;
  &:hover {
    text-decoration: underline;
  }
}

.shortcut {
  color: lightslategray;
}
</style>
