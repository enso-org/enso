<script setup lang="ts">
/// <reference types="@histoire/plugin-vue/components" />

import { ref } from 'vue'

import { logEvent } from 'histoire/client'

import DropdownWidget from '@/components/widgets/DropdownWidget.vue'

const color = ref('#357ab9')
const selectedValue = ref('location')
const values = ref(['address', 'age', 'id', 'language', 'location', 'workplace'])
</script>

<template>
  <Story
    title="Dropdown"
    group="widgets"
    :layout="{ type: 'grid', width: 200 }"
    auto-props-disabled
  >
    <div style="height: 140px">
      <div style="position: relative">
        <DropdownWidget
          :color="color"
          :values="values"
          :selected-value="selectedValue"
          @click="(selectedValue = values[$event]!), logEvent('click', [$event])"
        />
      </div>
    </div>
    <template #controls>
      <HstColorSelect v-model="color" title="color" />
      <HstSelect v-model="selectedValue" title="selectedValue" :options="values" />
      <HstJson v-model="values" title="values" />
    </template>
  </Story>
</template>
