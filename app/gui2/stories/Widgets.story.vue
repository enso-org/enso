<script setup lang="ts">
import { logEvent } from 'histoire/client'
import { computed, ref } from 'vue'

import CheckboxWidget from '@/components/widgets/CheckboxWidget.vue'
import DropdownWidget from '@/components/widgets/DropdownWidget.vue'
import NumericInputWidget from '@/components/widgets/NumericInputWidget.vue'

// === Checkbox props ===

const checkboxState = ref(false)

// === Slider props ===

const state = ref(0)
const min = ref(0)
const max = ref(100)
const withLimits = ref(true)
const sliderLimits = computed(() => {
  return withLimits.value ? { min: min.value, max: max.value } : undefined
})

// === Dropdown props ===

const color = ref('#357ab9')
const backgroundColor = ref('#4778b4')
const selectedValue = ref('location')
const values = ref(['address', 'age', 'id', 'language', 'location', 'workplace'])
</script>

<template>
  <Story title="Widgets" group="graph" :layout="{ type: 'grid', width: 200 }" autoPropsDisabled>
    <Variant title="checkbox" :meta="{ customBackground: backgroundColor }">
      <CheckboxWidget v-model="checkboxState" />

      <template #controls>
        <HstCheckbox v-model="checkboxState" title="v-model" />
      </template>
    </Variant>
    <Variant title="numeric" :meta="{ customBackground: backgroundColor }">
      <NumericInputWidget v-model="state" :limits="sliderLimits" />

      <template #controls>
        <HstSlider v-model="state" title="v-model" :min="min" :max="max" />
        <HstCheckbox v-model="withLimits" title="With limits" />
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
