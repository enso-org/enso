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

const verteKey = ref(0)
watch(() => props.show, () => nextTick(() => verteKey.value++))
</script>

<template>
  <Verte v-show="props.show" 
    :key="verteKey"
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
