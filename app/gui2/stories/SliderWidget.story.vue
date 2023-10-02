<script setup lang="ts">
/// <reference types="@histoire/plugin-vue/components" />

import { ref } from 'vue'

import '@/assets/base.css'
import SliderWidget from '@/components/widgets/SliderWidget.vue'

const state = ref(0)
const min = ref(0)
const max = ref(100)
</script>

<template>
  <Story title="Slider" group="widgets" :layout="{ type: 'grid', width: 200 }">
    <SliderWidget v-model="state" :min="min" :max="max" />
  </Story>
</template>
