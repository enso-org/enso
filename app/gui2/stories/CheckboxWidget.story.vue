<script setup lang="ts">
/// <reference types="@histoire/plugin-vue/components" />

import { ref } from 'vue'

import { logEvent } from 'histoire/client'

import CheckboxWidget from '@/components/widgets/CheckboxWidget.vue'

const checkboxState = ref(false)
</script>

<template>
  <Story
    title="Checkbox"
    group="widgets"
    :layout="{ type: 'grid', width: 200 }"
    auto-props-disabled
  >
    <CheckboxWidget
      v-model="checkboxState"
      @update:modelValue="logEvent('update:modelValue', [$event])"
    />

    <template #controls>
      <HstCheckbox v-model="checkboxState" title="v-model" />
    </template>
  </Story>
</template>
