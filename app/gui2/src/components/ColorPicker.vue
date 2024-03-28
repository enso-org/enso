<script setup lang="ts">
import { convertToRgb } from '@/util/colors'
import Verte from 'verte-vue3'
import 'verte-vue3/dist/verte.css'
import { nextTick, ref, watch } from 'vue';

const props = defineProps<{ show: boolean; color: string }>()
const emit = defineEmits<{ 'update:color': [string] }>()

const colorModel = ref(convertToRgb(props.color))
watch(() => props.color, (c) => { colorModel.value = convertToRgb(c) })
watch(colorModel, (c) => c != null && props.show && emit('update:color', c))

/** Looks weird, but it is a fix for verte’s bug: https://github.com/baianat/verte/issues/52. */
const key = ref(0)
watch(() => props.show, () => nextTick(() => key.value++))
</script>

<template>
  <Verte v-show="props.show" 
    :key="key"
    v-model="colorModel"
    picker="square"
    model="rgb"
    display="widget"
    :draggable="false"
    :enableAlpha="false"
    @pointerdown.stop
    @pointerup.stop
    @click.stop
  />
</template>

<style>
</style>
