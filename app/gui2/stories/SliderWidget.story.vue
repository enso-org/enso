<script setup lang="ts">
import { ref } from 'vue'

import { logEvent } from 'histoire/client'

import SliderWidget from '@/components/widgets/SliderWidget.vue'

const state = ref(0)
const min = ref(0)
const max = ref(100)
</script>

<template>
  <Story title="Slider" group="widgets" :layout="{ type: 'grid', width: 200 }" auto-props-disabled>
    <SliderWidget
      v-model="state"
      :min="min"
      :max="max"
      @update:modelValue="logEvent('update:modelValue', [$event])"
    />

    <template #controls>
      <HstSlider v-model="state" title="v-model" :min="min" :max="max" />
      <HstNumber v-model="min" title="min" />
      <HstNumber v-model="max" title="max" />
    </template>
  </Story>
</template>
