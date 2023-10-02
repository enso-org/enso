<script setup lang="ts">
/// <reference types="@histoire/plugin-vue/components" />

import { ref } from 'vue'

import '@/assets/base.css'
import CheckboxWidget from '@/components/widgets/CheckboxWidget.vue'

const checkboxState = ref(false)
</script>

<template>
  <Story title="Checkbox" group="widgets" :layout="{ type: 'grid', width: 200 }">
    <CheckboxWidget v-model="checkboxState" />
  </Story>
</template>
