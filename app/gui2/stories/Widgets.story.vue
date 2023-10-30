<script setup lang="ts">
import { logEvent } from 'histoire/client'
import { ref } from 'vue'

import CheckboxWidget from '@/components/widgets/CheckboxWidget.vue'
import DropdownWidget from '@/components/widgets/DropdownWidget.vue'
import PlaceholderWidget from '@/components/widgets/PlaceholderWidget.vue'
import SliderWidget from '@/components/widgets/SliderWidget.vue'

// === Checkbox props ===

const checkboxState = ref(false)

// === Slider props ===

const state = ref(0)
const min = ref(0)
const max = ref(100)

// === Dropdown props ===

const color = ref('#357ab9')
const selectedValue = ref('location')
const values = ref(['address', 'age', 'id', 'language', 'location', 'workplace'])
</script>

<template>
  <Story title="Widgets" group="graph" :layout="{ type: 'grid', width: 200 }" autoPropsDisabled>
    <Variant title="placeholder">
      <PlaceholderWidget />
    </Variant>
    <Variant title="checkbox">
      <CheckboxWidget
        v-model="checkboxState"
        @update:modelValue="logEvent('update:modelValue', [$event])"
      />

      <template #controls>
        <HstCheckbox v-model="checkboxState" title="v-model" />
      </template>
    </Variant>
    <Variant title="slider">
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
    </Variant>
    <Variant title="dropdown">
      <div style="height: 140px">
        <div style="position: relative">
          <DropdownWidget
            :color="color"
            :values="values"
            :selectedValue="selectedValue"
            @click="(selectedValue = values[$event]!), logEvent('click', [$event])"
          />
        </div>
      </div>
      <template #controls>
        <HstColorSelect v-model="color" title="color" />
        <HstSelect v-model="selectedValue" title="selectedValue" :options="values" />
        <HstJson v-model="values" title="values" />
      </template>
    </Variant>
  </Story>
</template>
