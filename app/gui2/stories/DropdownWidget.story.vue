<script setup lang="ts">
/// <reference types="@histoire/plugin-vue/components" />

import { ref } from 'vue'

import '@/assets/base.css'
import DropdownWidget from '@/components/widgets/DropdownWidget.vue'

const color = ref('#357ab9')
const values = ref(['address', 'age', 'id', 'language', 'location', 'workplace'])
const selectedValue = ref('location')
</script>

<template>
  <Story group="widgets" :layout="{ type: 'grid', width: 200 }">
    <div style="height: 140px">
      <div style="position: relative">
        <DropdownWidget
          :color="color"
          :values="values"
          :selected-value="selectedValue"
          @click="selectedValue = values[$event]!"
        />
      </div>
    </div>
  </Story>
</template>
