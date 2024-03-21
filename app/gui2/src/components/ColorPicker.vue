<script setup lang="ts">
import { convertToRgb } from '@/util/colors'
import Verte from 'verte-vue3'
import 'verte-vue3/dist/verte.css'
import { nextTick, ref, watch } from 'vue';

const props = defineProps<{ show: boolean; color: string }>()
const emit = defineEmits<{ 'update:color': [string] }>()

const color = ref(convertToRgb(props.color))
const verteKey = ref(0)
watch(() => props.show, () => nextTick(() => verteKey.value++))
watch(color, (c) => c != null && props.show && emit('update:color', c))
</script>

<template>
  <Verte v-show="props.show" 
    :key="verteKey"
    v-model="color"
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
