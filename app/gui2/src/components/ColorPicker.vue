<script setup lang="ts">
import { convertToRgb } from '@/util/colors'
// @ts-ignore
import Verte from 'verte-vue3'
import 'verte-vue3/dist/verte.css'
import { computed, nextTick, ref, watch } from 'vue'

const props = defineProps<{ show: boolean; color: string }>()
const emit = defineEmits<{ 'update:color': [string] }>()

/** Comparing RGB colors is complicated, because the string representation always has some minor differences.
 * In this particular case, we remove spaces to match the format used by `verte-vue3`. */
const normalizedColor = computed(() => {
  return convertToRgb(props.color)?.replaceAll(/\s/g, '') ?? ''
})

const updateColor = (c: string) => {
  if (props.show && normalizedColor.value !== c) emit('update:color', c)
}

/** Looks weird, but it is a fix for verte’s bug: https://github.com/baianat/verte/issues/52. */
const key = ref(0)
watch(
  () => props.show,
  () => nextTick(() => key.value++),
)
</script>

<template>
  <Verte
    v-show="props.show"
    :key="key"
    :modelValue="convertToRgb(props.color)"
    picker="square"
    model="rgb"
    display="widget"
    :draggable="false"
    :enableAlpha="false"
    @update:modelValue="updateColor"
    @pointerdown.stop
    @pointerup.stop
    @click.stop
  />
</template>

<style></style>
