<script setup lang="ts">
import { ref, watch, type ComponentInstance } from 'vue'
import AutoSizedInput from './widgets/AutoSizedInput.vue'

const model = defineModel<string>({ required: true })
const _props = defineProps<{ active: boolean; editing: boolean }>()

const input = ref<ComponentInstance<typeof AutoSizedInput>>()
watch(input, (input, old) => {
  if (old == null && input != null) input.focus()
})
</script>

<template>
  <div :class="['NavBreadcrumb', { inactive: !active }]">
    <AutoSizedInput v-if="editing" ref="input" v-model.lazy="model" :autoSelect="true" />
    <span v-else v-text="model"></span>
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
