<script setup lang="ts">
import { ref, computed } from 'vue'
import { PointerButtonMask, usePointer } from '@/util/events'

const props = defineProps<{ modelValue: number; min: number; max: number }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: number] }>()

const sliderNode = ref<HTMLElement>()

const dragPointer = usePointer((position) => {
  if (sliderNode.value == null) {
    return
  }
  const rect = sliderNode.value.getBoundingClientRect()
  const fractionRaw = (position.absolute.x - rect.left) / (rect.right - rect.left)
  const fraction = Math.max(0, Math.min(1, fractionRaw))
  const newValue = props.min + Math.round(fraction * (props.max - props.min))
  emit('update:modelValue', newValue)
}, PointerButtonMask.Main)

const sliderWidth = computed(
  () => `${((props.modelValue - props.min) * 100) / (props.max - props.min)}%`,
)

const inputValue = computed({
  get() {
    return props.modelValue
  },
  set(value) {
    emit('update:modelValue', value)
  },
})
</script>

<template>
  <div ref="sliderNode" class="Slider" v-on="dragPointer.events">
    <div class="fraction" :style="{ width: sliderWidth }"></div>
    <input v-model.number="inputValue" type="number" :size="1" class="value" />
  </div>
</template>

<style scoped>
.Slider {
  clip-path: inset(0 round var(--radius-full));
  position: relative;
  user-select: none;
  display: flex;
  justify-content: space-around;
  background: var(--color-widget);
  border-radius: var(--radius-full);
  width: 56px;
}

.fraction {
  position: absolute;
  height: 100%;
  left: 0;
  background: var(--color-widget);
}

.value {
  position: relative;
  display: inline-block;
  background: none;
  border: none;
  text-align: center;
  min-width: 0;
  font-weight: 800;
  line-height: 171.5%;
  height: 24px;
  padding-top: 1px;
  padding-bottom: 1px;
  appearance: textfield;
  -moz-appearance: textfield;
}

input::-webkit-outer-spin-button,
input::-webkit-inner-spin-button {
  -webkit-appearance: none;
  margin: 0;
}
</style>
